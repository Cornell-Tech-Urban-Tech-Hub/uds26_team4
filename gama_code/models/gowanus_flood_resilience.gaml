model gowanus_flood_resilience

global {
    file dem_file      <- file("../data/processed/gowanus/gowanus_dem_2263.tif");
    file lots_file     <- file("../data/processed/gowanus/gowanus_lots_surface_2263.geojson");
    file buildings_file<- file("../data/processed/gowanus/gowanus_buildings_enriched_2263.geojson");
    file canal_file    <- file("../data/processed/gowanus/gowanus_canal_water_no_345_2263.geojson");
    file compare_green_file <- file("../data/processed/gowanus/scenarios/green_infrastructure.shp");
    file compare_block_file <- file("../data/processed/gowanus/scenarios/flood_barrier.shp");
    file compare_mixed_block_file <- file("../data/processed/gowanus/scenarios/mixed_barrier.shp");

    geometry shape <- envelope(dem_file);

    float water_level            <- 5.0;
    float rain_speed             <- 0.2;
    bool  rain_paused            <- false;
    float building_height_factor <- 1.0;
    float pollution_factor       <- 1.0;
    float canal_pollution_level  <- 7.0;
    float pollution_mobility     <- 1.4;
    float green_filter_rate      <- 0.08;
    float baseline_pollution_push <- 0.0;
    string scenario_name         <- "baseline";
    int flooded_cells_count      <- 0;
    float mean_water_depth       <- 0.0;
    int polluted_cells_count     <- 0;
    float mean_dissolved_pollution <- 0.0;
    float mean_cumulative_pollution <- 0.0;
    float pollution_on_green     <- 0.0;

    init {
        create lot_surface from: lots_file with: [
            land_use_label:: string(read("land_use_label")),
            surface_type:: string(read("surface_type")),
            surface_label:: string(read("surface_label")),
            infiltration_coeff:: float(read("infiltration_coeff")),
            display_r:: int(read("display_r")),
            display_g:: int(read("display_g")),
            display_b:: int(read("display_b")),
            display_a:: int(read("display_a"))
        ];

        create building_mass from: buildings_file with: [
            building_id:: string(read("building_id")),
            height_roof_m:: float(read("height_roof_m")),
            num_floors:: float(read("num_floors"))
        ];

        create canal_zone from: canal_file;
        create scenario_green from: compare_green_file with: [feature_height:: float(read("height"))];
        create scenario_block from: compare_block_file with: [feature_height:: float(read("height"))];
        create scenario_mixed_block from: compare_mixed_block_file with: [feature_height:: float(read("height"))];

        ask cell {
            if (altitude <= 0.0 and length(neighbors) < 8) {
                outside_domain <- true;
            }

            list<lot_surface> ls <- lot_surface overlapping self;
            if (not empty(ls)) {
                surface_type    <- ls[0].surface_type;
                land_use_label  <- ls[0].land_use_label;
                absorption_rate <- ls[0].infiltration_coeff * 2.0;
                runoff_coeff    <- 1.0 - min(0.9, ls[0].infiltration_coeff);
                is_green        <- (surface_type = "green_open_space") or (surface_type = "tree_canopy");

                if (surface_type = "road_surface" or surface_type = "other_paved_surface" or surface_type = "parking_paved") {
                    absorption_rate <- absorption_rate * 0.4;
                    runoff_coeff    <- max(runoff_coeff, 0.85);
                } else if (surface_type = "building_hardscape") {
                    absorption_rate <- absorption_rate * 0.2;
                    runoff_coeff    <- max(runoff_coeff, 0.95);
                } else if (surface_type = "industrial_hardscape") {
                    absorption_rate <- absorption_rate * 0.3;
                    runoff_coeff    <- max(runoff_coeff, 0.9);
                } else if (surface_type = "green_open_space" or surface_type = "tree_canopy") {
                    absorption_rate <- absorption_rate * 1.3;
                    runoff_coeff    <- min(runoff_coeff, 0.35);
                }

                retention_coeff <- 0.03;
                if (surface_type = "road_surface" or surface_type = "other_paved_surface" or surface_type = "parking_paved") {
                    retention_coeff <- 0.05;
                } else if (surface_type = "building_hardscape" or surface_type = "industrial_hardscape") {
                    retention_coeff <- 0.06;
                } else if (surface_type = "green_open_space" or surface_type = "tree_canopy") {
                    retention_coeff <- 0.02;
                }
            }

            list<building_mass> bs <- building_mass overlapping self;
            if (not empty(bs)) {
                float max_h <- max(bs collect each.height_roof_m);
                is_building     <- true;
                building_height <- max_h * building_height_factor;
                altitude        <- altitude + building_height;
            }

            // Comparison scenarios:
            // green: increase infiltration on selected polygons
            // block: raise local terrain as hard barrier
            // mixed: combine green infrastructure polygons and mixed barrier polygons
            if (scenario_name = "green" or scenario_name = "mixed") {
                list<scenario_green> gs <- scenario_green overlapping self;
                if (!empty(gs)) {
                    is_green <- true;
                    absorption_rate <- max(absorption_rate, 0.32 + (gs[0].feature_height * 0.02));
                    runoff_coeff <- min(runoff_coeff, 0.22);
                    retention_coeff <- min(retention_coeff, 0.018);
                }
            }

            if (scenario_name = "block" or scenario_name = "mixed") {
                if (scenario_name = "block") {
                    list<scenario_block> bs_override <- scenario_block overlapping self;
                    if (!empty(bs_override)) {
                        float extra_block_height <- max(8.0, bs_override[0].feature_height * 0.35);
                        altitude <- altitude + extra_block_height;
                        runoff_coeff <- max(runoff_coeff, 0.97);
                        absorption_rate <- absorption_rate * 0.15;
                        retention_coeff <- max(retention_coeff, 0.065);
                        is_block_control <- true;
                    }
                } else {
                    list<scenario_mixed_block> mixed_override <- scenario_mixed_block overlapping self;
                    if (!empty(mixed_override)) {
                        float extra_block_height <- max(8.0, mixed_override[0].feature_height * 0.35);
                        altitude <- altitude + extra_block_height;
                        runoff_coeff <- max(runoff_coeff, 0.97);
                        absorption_rate <- absorption_rate * 0.15;
                        retention_coeff <- max(retention_coeff, 0.065);
                        is_block_control <- true;
                    }
                }
            }

            list<canal_zone> cz <- canal_zone overlapping self;
            if (!outside_domain and not empty(cz)) {
                is_canal_source <- true;
                has_water   <- true;
                water_depth <- max(0.5, 5.0 - altitude);
                local_water <- water_depth;
                dissolved_pollution <- canal_pollution_level * pollution_factor * pollution_mobility;
                sediment <- canal_pollution_level * 0.5 * pollution_factor;
                cumulative_pollution <- canal_pollution_level * pollution_factor * 0.6;
            }

        }

        ask cell { do refresh_visual; }
        ask cell where (each.outside_domain = false and each.is_building = false) {
            list<cell> terrain_neighbors <- neighbors where (!each.outside_domain);
            if (!empty(terrain_neighbors)) {
                depression_index <- max(0.0, mean(terrain_neighbors collect each.altitude) - altitude);
            }
        }
        do update_metrics;

        write "Loaded lots: " + length(lot_surface);
        write "Loaded buildings: " + length(building_mass);
        write "Building cells: " + (cell count (each.is_building));
        write "Green cells: " + (cell count (each.is_green));
        write "Canal source cells: " + (cell count (each.is_canal_source));
    }

    reflex flow_logic {
        if (!rain_paused) {
            water_level <- water_level + rain_speed;
        } else {
            water_level <- max(0.0, water_level - 0.03);
        }

        ask cell {
            incoming_water <- 0.0;
            incoming_pollution <- 0.0;
        }

        // Pluvial flooding: low-lying cells accumulate rainfall locally even before canal water reaches them.
        ask cell where (each.outside_domain = false and each.is_building = false) {
            if (!rain_paused) {
                float previous_water <- local_water;
                float rain_capture <- max(0.0, (rain_speed * runoff_coeff * min(0.35, depression_index * 0.10)) - (absorption_rate * 0.06));
                if (rain_capture > 0.0) {
                    local_water <- local_water + rain_capture;
                    if (!is_canal_source and previous_water > 0.0 and dissolved_pollution > 0.0) {
                        // Fresh rainwater dilutes existing contamination in pluvial ponding areas.
                        dissolved_pollution <- dissolved_pollution * (previous_water / max(0.001, local_water));
                    }
                    water_depth <- local_water;
                    if (local_water > 0.02) {
                        has_water <- true;
                    }
                }
            }
        }

        // A small downslope transfer makes water accumulate more clearly in local depressions.
        ask cell where (each.outside_domain = false and each.is_building = false and each.has_water and each.local_water > 0.03) {
            float current_head <- altitude + local_water;
            list<cell> lower_neighbors <- neighbors where (!each.outside_domain and !each.is_building and ((each.altitude + each.local_water) < (current_head - 0.03)));

            if (!empty(lower_neighbors)) {
                float source_resistance <- is_green ? 0.55 : 1.0;
                float transferable <- min(local_water * 0.12 * source_resistance, max(0.0, local_water - 0.02));
                float total_drop <- sum(lower_neighbors collect max(0.001, current_head - (each.altitude + each.local_water)));

                if (transferable > 0.0 and total_drop > 0.0) {
                    float moved_water <- 0.0;
                    loop target over: lower_neighbors {
                        float drop <- max(0.001, current_head - (target.altitude + target.local_water));
                        float target_resistance <- target.is_green ? 0.60 : 1.0;
                        float share <- transferable * (drop / total_drop) * target_resistance;
                        moved_water <- moved_water + share;
                        ask target {
                            incoming_water <- incoming_water + share;
                            incoming_pollution <- incoming_pollution + (share * myself.dissolved_pollution / max(0.001, myself.local_water));
                        }
                    }
                    local_water <- local_water - moved_water;
                }
            }
        }

        ask cell where (each.outside_domain = false and each.is_building = false and (each.incoming_water > 0.0 or each.incoming_pollution > 0.0)) {
            local_water <- local_water + incoming_water;
            if (local_water > 0.02) {
                has_water <- true;
            }
            dissolved_pollution <- ((dissolved_pollution * max(0.0, local_water - incoming_water)) + incoming_pollution) / max(0.001, local_water);
            water_depth <- local_water;
        }

        ask cell where (each.outside_domain = false and each.is_building = false and each.has_water = false) {
            list<cell> wet_neighbors <- neighbors where (!each.outside_domain and !each.is_building and each.has_water);
            bool neighbor_wet <- !empty(wet_neighbors);

            if (!rain_paused and neighbor_wet and altitude < (water_level + 0.25)) {
                float potential_depth <- ((water_level - altitude) * runoff_coeff) - (absorption_rate * (rain_paused ? 1.0 : 0.4));

                if (potential_depth > 0.01) {
                    float upstream_peak <- max(wet_neighbors collect each.dissolved_pollution);
                    float upstream_mean <- mean(wet_neighbors collect each.dissolved_pollution);
                    float inherited_pollution <- max(upstream_peak * 0.46, upstream_mean * 0.68) * pollution_mobility;
                    if (scenario_name = "baseline") {
                        inherited_pollution <- inherited_pollution * (1.0 + baseline_pollution_push);
                    }

                    has_water <- true;
                    water_depth <- potential_depth;
                    local_water <- potential_depth;
                    dissolved_pollution <- inherited_pollution;
                    cumulative_pollution <- cumulative_pollution + (inherited_pollution * 0.25);
                }
            }
        }

        ask cell where (each.has_water and each.outside_domain = false and each.is_building = false) {
            list<cell> wet_neighbors <- neighbors where (!each.outside_domain and !each.is_building and each.has_water);

            if (is_canal_source) {
                if (!rain_paused) {
                    local_water <- max(local_water, 0.8);
                    dissolved_pollution <- max(dissolved_pollution, canal_pollution_level * pollution_factor * pollution_mobility);
                }
            }

            if (!rain_paused) {
                if (is_canal_source or !empty(wet_neighbors)) {
                    float target_depth <- ((water_level - altitude) * runoff_coeff) - (absorption_rate * 0.35);
                    if (is_canal_source) {
                        target_depth <- target_depth + (rain_speed * 0.18);
                    }
                    local_water <- max(local_water, target_depth);
                }
            } else {
                local_water <- max(0.0, local_water - (absorption_rate + (is_green ? 0.02 : 0.008)));
            }

            if (!empty(wet_neighbors)) {
                float upstream_peak <- max(wet_neighbors collect each.dissolved_pollution);
                float upstream_mean <- mean(wet_neighbors collect each.dissolved_pollution);
                float propagated_pollution <- max(upstream_peak * 0.34, upstream_mean * 0.56);
                if (scenario_name = "baseline") {
                    propagated_pollution <- propagated_pollution * (1.0 + baseline_pollution_push * 0.6);
                }
                float mixing_water <- min(local_water, max(0.07, mean(wet_neighbors collect each.local_water) * 0.34));
                dissolved_pollution <- ((dissolved_pollution * max(0.0, local_water - mixing_water)) + (propagated_pollution * mixing_water)) / max(0.001, local_water);
            }

            float settling_factor <- rain_paused ? 0.08 : 0.018;
            if (surface_type = "road_surface" or surface_type = "other_paved_surface" or surface_type = "parking_paved") {
                settling_factor <- settling_factor * 0.55;
            } else if (surface_type = "green_open_space" or surface_type = "tree_canopy") {
                settling_factor <- settling_factor * 1.45;
            }
            if (is_block_control) {
                settling_factor <- settling_factor * 1.35;
            }
            sediment <- sediment + (dissolved_pollution * retention_coeff * runoff_coeff * settling_factor);

            float pollution_decay <- (retention_coeff * (rain_paused ? 0.02 : 0.005)) + (is_green ? (rain_paused ? 0.018 : 0.006) : 0.0);
            if (scenario_name = "baseline") {
                pollution_decay <- pollution_decay * 0.92;
            }
            dissolved_pollution <- max(0.0, dissolved_pollution - pollution_decay);
            float cumulative_gain <- dissolved_pollution * (rain_paused ? 0.022 : 0.04);
            if (scenario_name = "baseline") {
                cumulative_gain <- cumulative_gain * 1.15;
            }
            cumulative_pollution <- cumulative_pollution + cumulative_gain;

            if (is_green) {
                dissolved_pollution <- max(0.0, dissolved_pollution - (green_filter_rate * (rain_paused ? 1.4 : 0.85)));
                cumulative_pollution <- cumulative_pollution * (rain_paused ? 0.93 : 0.965);
            }

            water_depth <- local_water;
            if (water_depth <= 0.0) {
                has_water <- false;
            }
        }

        ask cell where (each.outside_domain = false and each.has_water = false and (each.sediment > 0.0 or each.dissolved_pollution > 0.0 or each.cumulative_pollution > 0.0)) {
            float decay <- 0.005 + (is_green ? absorption_rate * 0.06 : 0.0) + (!is_green and runoff_coeff < 0.5 ? 0.01 : 0.0);
            sediment <- max(0.0, sediment - decay);
            dissolved_pollution <- max(0.0, dissolved_pollution - (decay * 0.5));
            cumulative_pollution <- max(0.0, cumulative_pollution - (decay * 0.15));
        }

        ask cell { do refresh_visual; }
        do update_metrics;

    }

    action update_metrics {
        list<cell> flooded_cells <- cell where (each.outside_domain = false and each.has_water and each.local_water > 0.015);
        list<cell> polluted_cells <- cell where (each.outside_domain = false and ((each.sediment + each.dissolved_pollution + each.cumulative_pollution) > 0.04));
        list<cell> green_cells <- cell where (each.outside_domain = false and each.is_green);

        flooded_cells_count <- length(flooded_cells);
        polluted_cells_count <- length(polluted_cells);

        if (empty(flooded_cells)) {
            mean_water_depth <- 0.0;
        } else {
            mean_water_depth <- mean(flooded_cells collect each.local_water);
        }

        if (empty(polluted_cells)) {
            mean_dissolved_pollution <- 0.0;
            mean_cumulative_pollution <- 0.0;
        } else {
            mean_dissolved_pollution <- mean(polluted_cells collect each.dissolved_pollution);
            mean_cumulative_pollution <- mean(polluted_cells collect each.cumulative_pollution);
        }

        list<cell> polluted_green_cells <- green_cells where ((each.sediment + each.dissolved_pollution + each.cumulative_pollution) > 0.04);
        if (empty(polluted_green_cells)) {
            pollution_on_green <- 0.0;
        } else {
            pollution_on_green <- mean(polluted_green_cells collect (each.sediment + each.dissolved_pollution + each.cumulative_pollution));
        }
    }

    reflex update_patches {
        ask sediment_patch {
            cell host <- one_of (cell overlapping self);
            if (host != nil and (host.sediment >= 0.02 or host.dissolved_pollution >= 0.02 or host.cumulative_pollution >= 0.05)) {
                level <- host.sediment;
                dissolved_level <- host.dissolved_pollution;
                cumulative_level <- host.cumulative_pollution;
                location <- host.location;
                shape <- host.shape;
            } else {
                do die;
            }
        }

        ask cell where (each.outside_domain = false and (each.sediment >= 0.02 or each.dissolved_pollution >= 0.02 or each.cumulative_pollution >= 0.05)) {
            if (empty(sediment_patch overlapping self)) {
                create sediment_patch with: [
                    location: self.location,
                    shape: self.shape,
                    level: self.sediment,
                    dissolved_level: self.dissolved_pollution,
                    cumulative_level: self.cumulative_pollution
                ];
            }
        }
    }
}

species lot_surface {
    string land_use_label;
    string surface_type;
    string surface_label;
    float infiltration_coeff;
    int display_r;
    int display_g;
    int display_b;
    int display_a;

    aspect default {
        draw shape color: rgb(display_r, display_g, display_b, 50) border: rgb(255, 255, 255, 10);
    }
}

species canal_zone {
    aspect default {
        draw shape color: rgb(30, 144, 255, 18) border: rgb(30, 144, 255, 40);
    }
}

species building_mass {
    string building_id;
    float height_roof_m;
    float num_floors;

    aspect default {
        draw shape color: rgb(92, 96, 108, 205) border: rgb(40, 40, 48, 180);
    }
}

species scenario_green {
    float feature_height;

    aspect default {
        if (scenario_name = "green" or scenario_name = "mixed") {
            draw shape color: rgb(84, 186, 96, 75) border: rgb(34, 110, 42, 180);
        }
    }
}

species scenario_block {
    float feature_height;

    aspect default {
        if (scenario_name = "block") {
            draw shape color: rgb(72, 94, 112, 130) border: rgb(18, 28, 40, 230);
        }
    }
}

species scenario_mixed_block {
    float feature_height;

    aspect default {
        if (scenario_name = "mixed") {
            draw shape color: rgb(72, 94, 112, 130) border: rgb(18, 28, 40, 230);
        }
    }
}

species sediment_patch {
    float level;
    float dissolved_level;
    float cumulative_level;
    aspect default {
        rgb col;
        float total_level <- level + (dissolved_level * 0.6) + (cumulative_level * 0.35);

        if (total_level < 0.5) {
            col <- rgb(255, 230, 50, 90);
        } else if (total_level < 1.0) {
            col <- rgb(255, 165, 30, 105);
        } else if (total_level < 2.0) {
            col <- rgb(220, 80, 20, 120);
        } else if (total_level < 4.0) {
            col <- rgb(180, 30, 10, 135);
        } else {
            col <- rgb(120, 0, 0, 150);
        }

        draw shape color: col;
    }
}

grid cell file: dem_file {
    float altitude;
    float water_depth;
    float local_water;
    float absorption_rate;
    float runoff_coeff;
    float retention_coeff;
    float dissolved_pollution;
    float incoming_water;
    float incoming_pollution;
    float sediment;
    float cumulative_pollution;
    float building_height;
    string land_use_label;
    string surface_type;
    bool is_green;
    bool has_water;
    bool is_building;
    bool is_canal_source;
    bool is_block_control;
    bool outside_domain;
    float depression_index;
    int water_alpha;

    init {
        altitude <- grid_value;
        water_depth <- 0.0;
        local_water <- 0.0;
        absorption_rate <- 0.0;
        runoff_coeff <- 1.0;
        retention_coeff <- 0.03;
        dissolved_pollution <- 0.0;
        incoming_water <- 0.0;
        incoming_pollution <- 0.0;
        sediment <- 0.0;
        cumulative_pollution <- 0.0;
        building_height <- 0.0;
        land_use_label <- "";
        surface_type <- "";
        is_green <- false;
        has_water <- false;
        is_building <- false;
        is_canal_source <- false;
        is_block_control <- false;
        outside_domain <- false;
        depression_index <- 0.0;
        water_alpha <- 0;
        color <- rgb(0, 102, 214, 0);
    }

    action refresh_visual {
        if (outside_domain) {
            water_alpha <- 0;
            color <- rgb(0, 102, 214, 0);
        } else if (has_water and local_water > 0.015) {
            water_alpha <- min(255, 150 + int(local_water * 55));
            if (local_water < 0.10) {
                color <- rgb(186, 224, 255, 160);
            } else if (local_water < 0.25) {
                color <- rgb(134, 196, 250, 175);
            } else if (local_water < 0.50) {
                color <- rgb(88, 166, 245, 190);
            } else if (local_water < 0.90) {
                color <- rgb(48, 132, 228, 205);
            } else {
                color <- rgb(19, 96, 198, 220);
            }
        } else {
            water_alpha <- 0;
            color <- rgb(0, 102, 214, 0);
        }
    }

    aspect default {
        if (!outside_domain) {
            float local_total <- (sediment * 0.85) + (dissolved_pollution * 0.55) + (cumulative_pollution * 0.03);
            float neighbor_total <- empty(neighbors)
                ? local_total
                : mean(neighbors collect ((each.sediment * 0.85) + (each.dissolved_pollution * 0.55) + (each.cumulative_pollution * 0.03)));
            float display_pollution <- max(local_total, (local_total * 0.6) + (neighbor_total * 0.4));
            bool near_wet <- !empty(neighbors where (each.has_water and !each.outside_domain));

            if (has_water and local_water > 0.015) {
                rgb water_or_plume <- #blue;

                if (local_water < 0.10) {
                    water_or_plume <- rgb(186, 224, 255, 160);
                } else if (local_water < 0.25) {
                    water_or_plume <- rgb(134, 196, 250, 175);
                } else if (local_water < 0.50) {
                    water_or_plume <- rgb(88, 166, 245, 190);
                } else if (local_water < 0.90) {
                    water_or_plume <- rgb(48, 132, 228, 205);
                } else {
                    water_or_plume <- rgb(19, 96, 198, 220);
                }

                if (display_pollution < 0.06) {
                    if (local_water < 0.10) {
                        water_or_plume <- rgb(186, 224, 255, 168);
                    } else if (local_water < 0.25) {
                        water_or_plume <- rgb(134, 196, 250, 182);
                    } else if (local_water < 0.50) {
                        water_or_plume <- rgb(88, 166, 245, 196);
                    } else if (local_water < 0.90) {
                        water_or_plume <- rgb(48, 132, 228, 210);
                    } else {
                        water_or_plume <- rgb(19, 96, 198, 224);
                    }
                } else if (display_pollution < 0.40) {
                    water_or_plume <- rgb(255, 228, 110, 120);
                } else if (display_pollution < 0.90) {
                    water_or_plume <- rgb(255, 174, 58, 108);
                } else if (display_pollution < 1.70) {
                    water_or_plume <- rgb(230, 104, 34, 122);
                } else {
                    water_or_plume <- rgb(176, 36, 18, 138);
                }

                draw shape color: water_or_plume border: #transparent;
            } else if (display_pollution > 0.04 or (near_wet and neighbor_total > 0.04)) {
                rgb residue_col <- #black;
                float residue_level <- max(display_pollution, neighbor_total * 0.85);

                if (residue_level < 0.06) {
                    residue_col <- rgb(255, 241, 190, 34);
                } else if (residue_level < 0.40) {
                    residue_col <- rgb(255, 220, 148, 60);
                } else if (residue_level < 0.90) {
                    residue_col <- rgb(246, 182, 102, 62);
                } else if (residue_level < 1.70) {
                    residue_col <- rgb(227, 136, 74, 78);
                } else {
                    residue_col <- rgb(142, 56, 38, 96);
                }

                draw shape color: residue_col border: #transparent;
            }
        }

    }
}

experiment main_experiment type: gui {
    parameter "Scenario" var: scenario_name among: ["baseline", "green", "block", "mixed"];
    parameter "Pause Rain" var: rain_paused <- false;
    parameter "Rain Speed" var: rain_speed <- 0.2 min: 0.0 max: 1.0 step: 0.05;
    parameter "Building Height Factor" var: building_height_factor <- 1.0 min: 0.0 max: 1.5 step: 0.1;
    parameter "Pollution Factor" var: pollution_factor <- 1.0 min: 0.2 max: 2.0 step: 0.1;
    parameter "Canal Pollution Level" var: canal_pollution_level <- 7.0 min: 0.5 max: 8.0 step: 0.5;
    parameter "Pollution Mobility" var: pollution_mobility <- 1.4 min: 0.8 max: 2.5 step: 0.1;
    parameter "Green Filter Rate" var: green_filter_rate <- 0.08 min: 0.0 max: 0.3 step: 0.01;
    parameter "Baseline Pollution Push" var: baseline_pollution_push <- 0.0 min: 0.0 max: 0.4 step: 0.02;

    output {
        monitor "Flooded Cells" value: flooded_cells_count;
        monitor "Mean Water Depth" value: mean_water_depth;
        monitor "Polluted Cells" value: polluted_cells_count;
        monitor "Mean Dissolved Pollution" value: mean_dissolved_pollution;
        monitor "Mean Cumulative Pollution" value: mean_cumulative_pollution;
        monitor "Pollution On Green" value: pollution_on_green;

        display map_display type: 2d {
            species lot_surface;
            species canal_zone;
            species cell aspect: default;
            species building_mass;
            species scenario_green;
            species scenario_block;
            species scenario_mixed_block;
        }

        display flood_metrics type: 2d {
            chart "Flooding" type: series style: line {
                data "Flooded Cells" value: flooded_cells_count color: #blue;
                data "Mean Water Depth" value: mean_water_depth color: #cyan;
            }
        }

        display pollution_extent_metrics type: 2d {
            chart "Pollution Extent" type: series style: line {
                data "Polluted Cells" value: polluted_cells_count color: #orange;
            }
        }

        display pollution_level_metrics type: 2d {
            chart "Pollution Level" type: series style: line {
                data "Mean Dissolved" value: mean_dissolved_pollution color: #red;
                data "Mean Cumulative" value: mean_cumulative_pollution color: #brown;
            }
        }

        display green_metrics type: 2d {
            chart "Green Effect" type: series style: line {
                data "Pollution On Green" value: pollution_on_green color: #green;
            }
        }
    }
}
