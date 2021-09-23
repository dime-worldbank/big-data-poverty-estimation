# Subset India road network file to points near roads

buffer_m <- 5000

# Load / Prep Data -------------------------------------------------------------
#### Survey data
survey_df <- readRDS(file.path(data_dir, SURVEY_NAME,
                               "FinalData", "Individual Datasets", 
                               "survey_socioeconomic.Rds"))

## Subset to India
survey_df <- survey_df %>%
  dplyr::filter(country_code %in% "IA")

## Spatially define
coordinates(survey_df) <- ~longitude+latitude
crs(survey_df) <- CRS("+init=epsg:4326")

## Buffer
survey_buff_df <- geo.buffer_chunks(survey_df[1:200,], r = buffer_m, chunk_size = 100)

## To sf and dissolve
survey_buff_sf_agg <- survey_buff_df %>% 
  st_as_sf() %>%
  st_union()

#### OSM Data
osm_df <- readRDS(file.path(osm_dir, "FinalData", "india-210101-free", "gis_osm_roads_free_1.Rds"))
osm_df <- osm_df %>% st_as_sf()

# Intersection -----------------------------------------------------------------
osm_df_subset <- st_intersection_chunks(osm_df, survey_buff_sf_agg, 1000)

# Export -----------------------------------------------------------------------
saveRDS(osm_df_subset, 
        file.path(osm_dir, "FinalData", "india-210101-free", 
                  paste0("gis_osm_roads_free_1_",buffer_m,"m",SURVEY_NAME,".Rds")))

