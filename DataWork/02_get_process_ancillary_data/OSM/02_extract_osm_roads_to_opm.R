# Extract OSM Roads to BISP Households

# Load Data --------------------------------------------------------------------
#### Roads
osm_roads_sdf <- readRDS(file.path(project_file_path, "Data", "OSM", "FinalData", "gis_osm_roads_free_1.Rds"))

#### OSM Coordinates
opm_coords <- read.csv(file.path(secure_file_path, "Data", "BISP", "FinalData - PII", "GPS_uid_crosswalk.csv"),
                       stringsAsFactors = F)

opm_coords$uid <- opm_coords$uid %>% as.numeric()

# Some coordinates are bad; remove those
opm_coords <- opm_coords[(opm_coords$latitude < 37) & (opm_coords$latitude > 23),]
opm_coords <- opm_coords[(opm_coords$longitude < 81) & (opm_coords$longitude > 65),]

coordinates(opm_coords) <- ~longitude+latitude
crs(opm_coords) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

# Reproject Data ---------------------------------------------------------------
osm_roads_sdf <- spTransform(osm_roads_sdf, CRS(PAK_UTM_PROJ))
opm_coords   <- spTransform(opm_coords, CRS(PAK_UTM_PROJ))

opm_coords_buff1km  <- gBuffer(opm_coords, width = 1*1000, byid = T)
opm_coords_buff2km  <- gBuffer(opm_coords, width = 2*1000, byid = T)
opm_coords_buff5km  <- gBuffer(opm_coords, width = 5*1000, byid = T)

## To sf
osm_roads_sf       <- osm_roads_sdf      %>% st_as_sf()
opm_coords_buff1km <- opm_coords_buff1km %>% st_as_sf()
opm_coords_buff2km <- opm_coords_buff2km %>% st_as_sf()
opm_coords_buff5km <- opm_coords_buff5km %>% st_as_sf()

# Cleanup OSM Type Names -------------------------------------------------------
## Rename
osm_roads_sf$fclass <- osm_roads_sf$fclass %>% as.character()
osm_roads_sf$fclass[grepl("trunk", osm_roads_sf$fclass)]     <- "trunk"
osm_roads_sf$fclass[grepl("motorway", osm_roads_sf$fclass)]  <- "motorway"
osm_roads_sf$fclass[grepl("primary", osm_roads_sf$fclass)]   <- "primary"
osm_roads_sf$fclass[grepl("secondary", osm_roads_sf$fclass)] <- "secondary"
osm_roads_sf$fclass[grepl("tertiary", osm_roads_sf$fclass)]  <- "tertiary"
osm_roads_sf$fclass[grepl("track", osm_roads_sf$fclass)]     <- "track"

## Rename
osm_roads_sf <- osm_roads_sf[!(osm_roads_sf$fclass %in% c("steps",
                                                          "unknown",
                                                          "bridleway",
                                                          "cycleway")),]

osm_roads_sf$fclass %>% table()

# Length of Roads of Different Types -------------------------------------------
buffer_sf <- opm_coords_buff5km
road_type <- "secondary"

extract_road_length <- function(road_type, osm_roads_sf, buffer_sf){
  
  print(paste(road_type," =============="))
  
  ## Subset roads to road type and cut up so only in buffered portions
  # TODO: distance faster than intersection?
  buffer_sf_agg <- buffer_sf %>% st_union()
  osm_roads_sf_typei <- osm_roads_sf[osm_roads_sf$fclass %in% road_type,]
  osm_roads_sf_typei_subset <- st_intersection_chunks(osm_roads_sf_typei, buffer_sf_agg, 2000)
  
  ## Loop through buffers and extract road length
  road_length <- lapply(1:nrow(buffer_sf), function(i){
    print(paste0(i, " - Extracting Length: ", road_type))
    
    intersects_tf <- st_intersects(opm_coords_buff[i,], osm_roads_sf_typei_subset, sparse = F) %>% as.vector()
    road_in_buffer <- st_intersection(osm_roads_sf_typei_subset[intersects_tf,], opm_coords_buff[1,])
    
    if(nrow(road_in_buffer) > 0){
      out <- road_in_buffer %>% st_length() %>% sum() %>% as.numeric()
    } else{
      out <- 0
    }
    
    return(out)
  }) %>% unlist()
  
  ## Cleanup output
  buffer_sf[[road_type]] <- road_length
  out_df <- buffer_sf[,c("uid", road_type)]
  out_df$geometry <- NULL
  
  return(out_df)
}

# Implement and Export ---------------------------------------------------------
## 1km
rd_length_1km_df <- lapply(unique(osm_roads_sf$fclass),
                           extract_road_length,
                           osm_roads_sf,
                           opm_coords_buff1km) %>%
  reduce(merge, by = "uid") %>%
  dplyr::rename_at(vars(-uid), ~ paste0("osm_length_", ., "_1kmbuff"))
saveRDS(rd_length_1km_df, file.path(project_file_path, "Data", "OPM", "FinalData", "Individual Datasets", "osm_road_length_1kmbuff.Rds"))
write.csv(rd_length_1km_df, file.path(project_file_path, "Data", "OPM", "FinalData", "Individual Datasets", "osm_road_length_1kmbuff.csv"),
          row.names = F)

## 2km
rd_length_2km_df <- lapply(unique(osm_roads_sf$fclass),
                           extract_road_length,
                           osm_roads_sf,
                           opm_coords_buff2km) %>%
  reduce(merge, by = "uid") %>%
  dplyr::rename_at(vars(-uid), ~ paste0("osm_length_", ., "_2kmbuff"))
saveRDS(rd_length_2km_df, file.path(project_file_path, "Data", "OPM", "FinalData", "Individual Datasets", "osm_road_length_2kmbuff.Rds"))
write.csv(rd_length_2km_df, file.path(project_file_path, "Data", "OPM", "FinalData", "Individual Datasets", "osm_road_length_2kmbuff.csv"),
          row.names = F)

## 5km
rd_length_5km_df <- lapply(unique(osm_roads_sf$fclass),
                           extract_road_length,
                           osm_roads_sf,
                           opm_coords_buff5km) %>%
  reduce(merge, by = "uid") %>%
  dplyr::rename_at(vars(-uid), ~ paste0("osm_length_", ., "_5kmbuff"))
saveRDS(rd_length_5km_df, file.path(project_file_path, "Data", "OPM", "FinalData", "Individual Datasets", "osm_road_length_5kmbuff.Rds"))
write.csv(rd_length_5km_df, file.path(project_file_path, "Data", "OPM", "FinalData", "Individual Datasets", "osm_road_length_5kmbuff.csv"),
          row.names = F)








