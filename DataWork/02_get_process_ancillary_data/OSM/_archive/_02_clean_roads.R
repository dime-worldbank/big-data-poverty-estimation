# Clean OSM Roads

# Standardize fclass

## Load Data
osm_roads_sf <- readRDS(file.path(project_file_path, "Data", "OSM", "FinalData", "gis_osm_roads_free_1.Rds"))

## Rename
osm_roads_sf$fclass <- osm_roads_sf$fclass %>% as.character()
osm_roads_sf$fclass[grepl("trunk", osm_roads_sf$fclass)]     <- "trunk"
osm_roads_sf$fclass[grepl("motorway", osm_roads_sf$fclass)]  <- "motorway"
osm_roads_sf$fclass[grepl("primary", osm_roads_sf$fclass)]   <- "primary"
osm_roads_sf$fclass[grepl("secondary", osm_roads_sf$fclass)] <- "secondary"
osm_roads_sf$fclass[grepl("tertiary", osm_roads_sf$fclass)]  <- "tertiary"
osm_roads_sf$fclass[grepl("track", osm_roads_sf$fclass)]     <- "track"

## Remove
osm_roads_sf <- osm_roads_sf[!(osm_roads_sf$fclass %in% c("steps",
                                                          "unknown",
                                                          "bridleway",
                                                          "cycleway")),]

osm_roads_sf$fclass %>% table()

## Export
saveRDS(osm_roads_sf, file.path(project_file_path, "Data", "OSM", "FinalData - Cleaned", "gis_osm_roads_free_1.Rds"))


