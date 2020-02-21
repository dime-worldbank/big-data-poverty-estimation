# Extract OSM Roads to BISP Households

# Load Data --------------------------------------------------------------------
osm_roads_sdf <- readRDS(file.path(project_file_path, "Data", "FinalData", "OSM", "gis_osm_roads_free_1.Rds"))
bisp_coords <- read_dta(file.path(bisp_geocodes_file_path, "GPS_uid_crosswalk.dta"))

# Spatiall Define BISP Data ----------------------------------------------------
bisp_coords <- bisp_coords[!is.na(bisp_coords$GPSN),]
bisp_coords$latitude <- get_lat_lon(bisp_coords$GPSN)
bisp_coords$longitude <- get_lat_lon(bisp_coords$GPSE)
bisp_coords$uid <- bisp_coords$uid %>% as.numeric()

# Some coordinates are bad; remove those
bisp_coords <- bisp_coords[(bisp_coords$latitude < 37) & (bisp_coords$latitude > 23),]
bisp_coords <- bisp_coords[(bisp_coords$longitude < 81) & (bisp_coords$longitude > 65),]

summary(bisp_coords$latitude)

coordinates(bisp_coords) <- ~longitude+latitude
crs(bisp_coords) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

# Reproject Data ---------------------------------------------------------------
osm_roads_sdf <- spTransform(osm_roads_sdf, CRS(PAK_UTM_PROJ))
bisp_coords <- spTransform(bisp_coords, CRS(PAK_UTM_PROJ))

# Clean OSM Data ---------------------------------------------------------------
#### Rename link road to main road type
osm_roads_sdf$fclass[osm_roads_sdf$fclass %in% "trunk_link"] <- "trunk"
osm_roads_sdf$fclass[osm_roads_sdf$fclass %in% "motorway_link"] <- "motorway"
osm_roads_sdf$fclass[osm_roads_sdf$fclass %in% "primary_link"] <- "primary"
osm_roads_sdf$fclass[osm_roads_sdf$fclass %in% "secondary_link"] <- "secondary"
osm_roads_sdf$fclass[osm_roads_sdf$fclass %in% "tertiary_link"] <- "tertiary"

#### Restrict to roads
osm_roads_sdf <- osm_roads_sdf[osm_roads_sdf$fclass %in% c("trunk",
                                                           "motorway",
                                                           "primary",
                                                           "secondary",
                                                           "tertiary",
                                                           "service",
                                                           "motorway",
                                                           "residential",
                                                           "unclassified",
                                                           "living_street"),]

osm_roads_sdf$fclass <- osm_roads_sdf$fclass %>% as.character()
osm_roads_sdf$id_agg <- 1

# Distance to Road Types -------------------------------------------------------
for(road_fclass in unique(osm_roads_sdf$fclass)){
  print(paste(road_fclass, "-------------------------------------------------"))
  osm_roads_sdf_i <- osm_roads_sdf[osm_roads_sdf$fclass %in% road_fclass,]
  osm_roads_sdf_i <- raster_aggregate_chunks(osm_roads_sdf_i, 6000, T)
  
  bisp_coords[[paste0("dist_osm_fclass_",road_fclass,"_meters")]] <- gDistance_chunks(bisp_coords, osm_roads_sdf_i, 25)
  
}

# Export -----------------------------------------------------------------------
bisp_coords_df <- bisp_coords@data %>%
  dplyr::select(-GPSN, -GPSE)

saveRDS(bisp_coords_df, file.path(final_data_file_path, "BISP", "Individual Datasets", "bisp_osm_roads.Rds"))





