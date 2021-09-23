# Merge data from OSM

# Load data --------------------------------------------------------------------
poi_df <- file.path(data_dir, SURVEY_NAME, 
                    "FinalData", "Individual Datasets", "osm", "poi") %>%
  list.files(full.names = T,
             pattern = "*.Rds") %>%
  lapply(readRDS)
a <- poi_df %>% bind_rows()
for(i in 1:10){
  print(is.data.frame(poi_df[[i]]))
}


road_df <- file.path(data_dir, SURVEY_NAME, 
                    "FinalData", "Individual Datasets", "osm", "roads_density") %>%
  list.files(full.names = T) %>%
  str_subset("road") %>%
  map_df(readRDS)

# Clean POI --------------------------------------------------------------------
mk_binary <- function(x) as.numeric(x > 0)

n_pois_to_use <- poi_df %>%
  #dplyr::mutate(country_code = uid %>% substring(1,2)) %>%
  #group_by(country_code) %>%
  dplyr::select(contains("n_poi")) %>%
  mutate_if(is.numeric, mk_binary) %>%
  summarise_if(is.numeric, mean) %>%
  dplyr::mutate(id = 1) %>%
  pivot_longer(cols = -id) %>%
  dplyr::arrange(-value) %>%
  head(50) %>%
  pull(name)

dist_pois_to_use <- n_pois_to_use %>%
  str_replace_all("n_poi", "dist_poi")

poi_df$osm_n_poi_total <- poi_df %>%
  dplyr::select(contains("n_poi")) %>%
  apply(1,sum, na.rm=T)

poi_clean_df <- poi_df[,c("uid",
                          "osm_n_poi_total",
                          n_pois_to_use,
                          dist_pois_to_use)]

# Clean Road -------------------------------------------------------------------
# Not all countries have motorways, so combine with trunk

# Deal with NAs in motorways
road_df$osm_motorway_distance_5000m_buff[is.na(road_df$osm_motorway_distance_5000m_buff)] <- 999999999
road_df$osm_motorway_length_5000m_buff[is.na(road_df$osm_motorway_length_5000m_buff)] <- 0
road_df$osm_motorway_N_segments_5000m_buff[is.na(road_df$osm_motorway_N_segments_5000m_buff)] <- 0

# Combine with trunk
road_df$osm_trunk_distance_5000m_buff <- road_df[,c("osm_motorway_distance_5000m_buff", "osm_trunk_distance_5000m_buff")] %>%
  apply(1, min)

road_df <- road_df %>%
  dplyr::mutate(osm_trunk_length_5000m_buff = osm_trunk_length_5000m_buff + osm_motorway_length_5000m_buff,
                osm_trunk_N_segments_5000m_buff = osm_trunk_N_segments_5000m_buff + osm_motorway_N_segments_5000m_buff) %>%
  dplyr::select(-contains("motorway"))

# Merge Road and POI; Export ---------------------------------------------------
osm_df <- merge(road_df,
                poi_clean_df,
                by = "uid",
                all = T)

# Export -----------------------------------------------------------------------
saveRDS(osm_df, file.path(data_dir, SURVEY_NAME, 
                          "FinalData", "Individual Datasets", "osm.Rds"))

write.csv(osm_df, file.path(data_dir, SURVEY_NAME, 
                          "FinalData", "Individual Datasets", "osm.csv"),
          row.names = F)






