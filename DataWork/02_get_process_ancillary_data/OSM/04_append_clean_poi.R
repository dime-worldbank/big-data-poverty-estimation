# Merge data from OSM

# Load data --------------------------------------------------------------------
n_poi_df <- file.path(data_dir, SURVEY_NAME, 
                      "FinalData", "Individual Datasets", "osm", "poi") %>%
  list.files(full.names = T,
             pattern = "*.Rds") %>%
  str_subset("_n_poi_5000m") %>%
  map_df(readRDS)

dist_poi_df <- file.path(data_dir, SURVEY_NAME, 
                         "FinalData", "Individual Datasets", "osm", "poi") %>%
  list.files(full.names = T,
             pattern = "*.Rds") %>%
  str_subset("_dist_poi") %>%
  map_df(readRDS)

# Merge Data -------------------------------------------------------------------
poi_df <- n_poi_df %>%
  left_join(dist_poi_df, by = "uid")

# Clean POI --------------------------------------------------------------------
mk_binary <- function(x) as.numeric(x > 0)

# For each POI type, counts the total number of survey locations that have
# at least one of the POI types (eg, number of survey locations that are near
# at least one restaurant). We use the top 30 POI types.
n_pois_to_use <- poi_df %>%
  dplyr::select(contains("n_poi")) %>%
  mutate_if(is.numeric, mk_binary) %>%
  summarise_if(is.numeric, mean) %>%
  dplyr::mutate(id = 1) %>%
  pivot_longer(cols = -id) %>%
  dplyr::arrange(-value) %>%
  head(30) %>%
  pull(name)

dist_pois_to_use <- n_pois_to_use %>%
  str_replace_all("n_poi", "distmeters_poi") %>%
  str_replace_all("_[:digit:]{5}m_buff", "") %>%
  str_replace_all("_[:digit:]{4}m_buff", "") %>%
  str_replace_all("_[:digit:]{3}m_buff", "")

## Add total number of POIs
poi_df$osm_n_poi_total <- poi_df %>%
  dplyr::select(contains("n_poi")) %>%
  apply(1, sum, na.rm=T)

## Add distance to nearest POI
min_ignore_na <- function(x){
  min(x[!is.na(x)])
}

poi_df$osm_distmeters_poi_any <- poi_df %>%
  dplyr::select(contains("distmeters")) %>%
  apply(1, min_ignore_na)

poi_clean_df <- poi_df[,c("uid",
                          "osm_n_poi_total",
                          "osm_distmeters_poi_any",
                          n_pois_to_use,
                          dist_pois_to_use)]

# Export -----------------------------------------------------------------------
saveRDS(poi_clean_df, file.path(data_dir, SURVEY_NAME, 
                                "FinalData", "Individual Datasets", "osm_poi.Rds"))







