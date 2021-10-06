# Merge data from OSM

# Load data --------------------------------------------------------------------
road_density_df <- file.path(data_dir, SURVEY_NAME, 
                             "FinalData", "Individual Datasets", "osm", "roads_density") %>%
  list.files(full.names = T) %>%
  str_subset("road") %>%
  map_df(readRDS) %>%
  rename_at(vars(-uid), ~ paste0("osm_", .))

road_distance_df <- file.path(data_dir, SURVEY_NAME, 
                              "FinalData", "Individual Datasets", "osm", "roads_distance") %>%
  list.files(full.names = T) %>%
  str_subset("road") %>%
  map_df(readRDS)

# TODO: Why need this?
#road_density_df <- road_density_df[road_density_df$uid != "BO200800002008",]
#road_distance_df <- road_distance_df[road_distance_df$uid != "BO200800002008",]

# Merge Data -------------------------------------------------------------------
road_df <- road_density_df %>%
  left_join(road_distance_df, by = "uid")

# Combine Types ----------------------------------------------------------------
# Combine trunk and motorways

# Deal with NAs in motorways
road_df$osm_dist_motorway[is.na(road_df$osm_dist_motorway)] <- 999999999
road_df$osm_length_motorway_5000m[is.na(road_df$osm_length_motorway_5000m)] <- 0
road_df$osm_N_segments_motorway_5000m[is.na(road_df$osm_N_segments_motorway_5000m)] <- 0

# Combine with trunk
road_df$osm_trunk_motorway <- road_df[,c("osm_dist_motorway", "osm_dist_trunk")] %>%
  apply(1, min)

road_df <- road_df %>%
  dplyr::mutate(osm_length_trunk_5000m = osm_length_trunk_5000m + osm_length_motorway_5000m,
                osm_N_segments_trunk_5000m = osm_N_segments_trunk_5000m + osm_N_segments_motorway_5000m) %>%
  dplyr::select(-contains("motorway"))

# Deal with NAs ----------------------------------------------------------------
# trunk, path, pedestrian, footway

road_types <- road_df %>% 
  names() %>% 
  str_subset("osm_dist") %>% 
  str_replace_all("osm_dist_", "")

#### Length and N segments; just assign 0
for(t in road_types){
  road_df[[paste0("osm_length_",t,"_5000m")]][is.na(road_df[[paste0("osm_length_",t,"_5000m")]])] <- 0
  road_df[[paste0("osm_N_segments_",t,"_5000m")]][is.na(road_df[[paste0("osm_N_segments_",t,"_5000m")]])] <- 0
}

#### Distance; assign max distance to any road within the country
# Better approach may be to combine categories

# Assign max distance to any road type
if(SURVEY_NAME %in% "DHS"){
  road_df$country_code <- road_df$uid %>% substring(1,2)
} else{
  road_df$country_code <- "PK"
}

for(c in unique(road_df$country_code)){
  
  MAX_DIST <- road_df %>%
    dplyr::filter(country_code %in% c) %>%
    dplyr::select_at(vars(contains("dist"))) %>%
    max(na.rm=T)

  for(t in road_types){
    
    road_df[[paste0("osm_dist_",t)]][is.na(road_df[[paste0("osm_dist_",t)]]) & 
                                       road_df$country_code %in% c] <- MAX_DIST
  }
}

# Aggregate variables ----------------------------------------------------------
## Total road length
road_df$osm_length_all_5000m <- road_df %>%
  dplyr::select(contains("osm_length")) %>%
  apply(1, sum, na.rm=T)

## Total segments
road_df$osm_N_segments_any_5000m <- road_df %>%
  dplyr::select(contains("osm_N_segments")) %>%
  apply(1, sum, na.rm=T)

## Min distance to any road
road_df$osm_dist_any <- road_df %>%
  dplyr::select(contains("osm_dist")) %>%
  apply(1, min, na.rm=T)

# Export -----------------------------------------------------------------------
road_df$country_code <- NULL

saveRDS(road_df, file.path(data_dir, SURVEY_NAME, 
                           "FinalData", "Individual Datasets", "osm_road.Rds"))


