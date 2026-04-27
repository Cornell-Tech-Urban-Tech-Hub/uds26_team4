## Gowanus Model Data

This folder contains only the runtime data used by `models/gowanus_flood_resilience.gaml`.

Required base layers:

- `gowanus_dem_2263.tif` - elevation raster used by the model grid.
- `gowanus_lots_surface_2263.geojson` - lot surfaces with surface type, infiltration, and display attributes.
- `gowanus_buildings_enriched_2263.geojson` - building footprints with height attributes.
- `gowanus_canal_water_no_345_2263.geojson` - canal water source geometry.

Scenario layers:

- `scenarios/green_infrastructure.shp` - green infrastructure intervention polygons.
- `scenarios/flood_barrier.shp` - barrier intervention polygons.
- `scenarios/mixed_barrier.shp` - barrier polygons used by the mixed scenario.

Keep each shapefile together with its `.shx`, `.dbf`, and `.prj` sidecar files.
