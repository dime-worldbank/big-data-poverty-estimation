# Merge Data with Survey Data

INV_DATA_DIR <- file.path(data_dir, SURVEY_NAME, "FinalData", "Individual Datasets")

# [Load] Survey data -----------------------------------------------------------
survey_df <- readRDS(file.path(INV_DATA_DIR, "survey_socioeconomic.Rds"))

#survey_df <- survey_df[survey_df$most_recent_survey %in% T,]
#a <- survey_df[!(survey_df$uid %in% osm_poi_df$uid),]
#a$country_code %>% unique()

# [Load] Facebook --------------------------------------------------------------
fb_df <- readRDS(file.path(INV_DATA_DIR, "facebook_marketing_dau_mau.Rds"))

fb_df <- fb_df %>%
  dplyr::select(uid, radius, contains("estimate_mau_upper_bound_")) %>%
  rename_at(vars(-uid), ~ paste0("fb_", .))

# [Load] Facebook RWI ----------------------------------------------------------
fb_rwi_df <- readRDS(file.path(INV_DATA_DIR, "fb_relative_wealth.Rds"))

# [Load] OSM -------------------------------------------------------------------
osm_poi_df  <- readRDS(file.path(INV_DATA_DIR, "osm_poi.Rds"))
osm_road_df <- readRDS(file.path(INV_DATA_DIR, "osm_road.Rds"))

## Cleanup
osm_road_df <- osm_road_df %>%
  dplyr::select(-contains("osm_N_segments"))
names(osm_road_df) <- names(osm_road_df) %>%
  str_replace_all("dist", "distmeters_road")

# [Load] CNN Features ----------------------------------------------------------
cnn_rgb_df <- readRDS(file.path(INV_DATA_DIR, "cnn_features", 
                                "cnn_features_s2_rgb_pca.Rds"))

cnn_ndvi_df <- readRDS(file.path(INV_DATA_DIR, "cnn_features", 
                                 "cnn_features_s2_ndvi_pca.Rds"))

cnn_bu_df <- readRDS(file.path(INV_DATA_DIR, "cnn_features", 
                               "cnn_features_s2_bu_pca.Rds"))

# [Load] Globcover -------------------------------------------------------------
gc_df <- readRDS(file.path(INV_DATA_DIR, "globcover.Rds"))

# [Load] WorldClim -------------------------------------------------------------
wc_df <- readRDS(file.path(INV_DATA_DIR, "worldclim.Rds"))

# [Load] Satellite data from GEE -----------------------------------------------
#file.path(INV_DATA_DIR, "satellite_data_from_gee") %>% list.files()

##### ** VIIRS, Median #####
viirs_df <- readRDS(file.path(INV_DATA_DIR, "satellite_data_from_gee", 
                              paste0("viirs_",
                                     BUFFER_SATELLITE,
                                     "_ubuff",
                                     BUFFER_SATELLITE,
                                     "_rbuff",
                                     BUFFER_SATELLITE,".Rds")))

viirs_df <- viirs_df %>%
  rename_at(vars(-uid, -year), ~ paste0("viirs_", .))

##### ** VIIRS, Std. Dev #####
viirs_sd_df <- readRDS(file.path(INV_DATA_DIR, "satellite_data_from_gee", 
                                 paste0("viirs_sd_",
                                        BUFFER_SATELLITE,
                                        "_ubuff",
                                        BUFFER_SATELLITE,
                                        "_rbuff",
                                        BUFFER_SATELLITE,".Rds")))

viirs_sd_df <- viirs_sd_df %>%
  rename_at(vars(-uid, -year), ~ paste0("viirs_", .) %>% tolower())

##### ** Landsat 8 #####
l8_df <- readRDS(file.path(INV_DATA_DIR, "satellite_data_from_gee", 
                           paste0("l8_ubuff",BUFFER_SATELLITE,"_rbuff",BUFFER_SATELLITE,".Rds")))

l8_df <- l8_df %>%
  rename_at(vars(-uid, -year), ~ paste0("l8_", .))

##### ** World pop #####
## Year of survey
wp2km_df <- readRDS(file.path(INV_DATA_DIR, "satellite_data_from_gee", 
                              "worldpop_2000_ubuff2000_rbuff2000.Rds")) %>%
  dplyr::rename(worldpop_2km = sum)

wp5km_df <- readRDS(file.path(INV_DATA_DIR, "satellite_data_from_gee", 
                              "worldpop_5000_ubuff5000_rbuff5000.Rds")) %>%
  dplyr::rename(worldpop_5km = sum)

wp10km_df <- readRDS(file.path(INV_DATA_DIR, "satellite_data_from_gee", 
                               "worldpop_10000_ubuff10000_rbuff10000.Rds")) %>%
  dplyr::rename(worldpop_10km = sum)

## 2020
wp2km2020_df <- readRDS(file.path(INV_DATA_DIR, "satellite_data_from_gee", 
                                  "worldpop2020_2000_ubuff2000_rbuff2000.Rds")) %>%
  dplyr::rename(worldpop2020_2km = sum)

wp5km2020_df <- readRDS(file.path(INV_DATA_DIR, "satellite_data_from_gee", 
                                  "worldpop2020_5000_ubuff5000_rbuff5000.Rds")) %>%
  dplyr::rename(worldpop2020_5km = sum)

wp10km2020_df <- readRDS(file.path(INV_DATA_DIR, "satellite_data_from_gee", 
                                   "worldpop2020_10000_ubuff10000_rbuff10000.Rds")) %>%
  dplyr::rename(worldpop2020_10km = sum)

## Merge
wp_df <- list(wp2km_df,
              wp5km_df, 
              wp10km_df, 
              wp2km2020_df, 
              wp5km2020_df,
              wp10km2020_df) %>%
  reduce(full_join, by = c("uid", "year"))

##### ** Elevation/Slope #####
elev_df <- readRDS(file.path(INV_DATA_DIR, "satellite_data_from_gee", 
                             paste0("elevation_ubuff",
                                    BUFFER_SATELLITE,
                                    "_rbuff",
                                    BUFFER_SATELLITE,
                                    ".Rds"))) %>%
  dplyr::rename(elev = mean)

slope_df <- readRDS(file.path(INV_DATA_DIR, "satellite_data_from_gee", 
                              paste0("slope_ubuff",BUFFER_SATELLITE,"_rbuff",BUFFER_SATELLITE,".Rds"))) %>%
  dplyr::rename(slope = mean)

elevslope <- full_join(elev_df, slope_df, by = c("uid", "year")) %>%
  rename_at(vars(-uid, -year), ~ paste0("elevslope_", .))

##### ** Global human modification index #####
#gbmod_df <- readRDS(file.path(INV_DATA_DIR, "satellite_data_from_gee", 
#                              paste0("GlobalHumanModification_ubuff",BUFFER_SATELLITE,"_rbuff",BUFFER_SATELLITE,".Rds")))

#gbmod_df <- gbmod_df %>%
#  dplyr::rename(globalmod_mean = mean)

##### ** AOD #####
aod_df <- readRDS(file.path(INV_DATA_DIR, "satellite_data_from_gee", 
                            paste0("aod_ubuff",BUFFER_SATELLITE,"_rbuff",BUFFER_SATELLITE,".Rds")))

aod_df <- aod_df %>%
  dplyr::rename(pollution_aod_047 = Optical_Depth_047,
                pollution_aod_055 = Optical_Depth_055)

##### ** Weather #####
weather_q1 <- readRDS(file.path(INV_DATA_DIR, "satellite_data_from_gee", 
                                paste0("ecmwf_weather_q1_ubuff10000_rbuff10000.Rds"))) %>%
  rename_at(vars(-uid, -year), ~ paste0("weather_q1_", .))

weather_q2 <- readRDS(file.path(INV_DATA_DIR, "satellite_data_from_gee", 
                                paste0("ecmwf_weather_q2_ubuff10000_rbuff10000.Rds"))) %>%
  rename_at(vars(-uid, -year), ~ paste0("weather_q2_", .))

weather_q3 <- readRDS(file.path(INV_DATA_DIR, "satellite_data_from_gee", 
                                paste0("ecmwf_weather_q3_ubuff10000_rbuff10000.Rds"))) %>%
  rename_at(vars(-uid, -year), ~ paste0("weather_q3_", .))

weather_q4 <- readRDS(file.path(INV_DATA_DIR, "satellite_data_from_gee", 
                                paste0("ecmwf_weather_q4_ubuff10000_rbuff10000.Rds"))) %>%
  rename_at(vars(-uid, -year), ~ paste0("weather_q4_", .))

weather_annual <- readRDS(file.path(INV_DATA_DIR, "satellite_data_from_gee", 
                                    paste0("ecmwf_weather_ubuff10000_rbuff10000.Rds"))) %>%
  rename_at(vars(-uid, -year), ~ paste0("weather_all_", .))

weather <- weather_annual %>%
  left_join(weather_q1, by = c("uid", "year")) %>%
  left_join(weather_q2, by = c("uid", "year")) %>%
  left_join(weather_q3, by = c("uid", "year")) %>%
  left_join(weather_q4, by = c("uid", "year"))

##### ** Pollution #####
poll_prefix <- c("CH4", "CO", "HCHO", "NO2", "ozone", "SO2", "uv_aer") %>%
  paste(collapse = "|")

pollution_df <- file.path(INV_DATA_DIR, "satellite_data_from_gee") %>%
  list.files(pattern = "*.Rds",
             full.names = T) %>%
  str_subset(poll_prefix) %>%
  str_subset(as.character(BUFFER_SATELLITE)) %>%
  lapply(readRDS) %>%
  reduce(full_join, by = c("uid", "year")) %>%
  rename_at(vars(-uid, -year), ~ paste0("pollution_s5p_", .)) %>%
  dplyr::select(uid, year,
                pollution_s5p_absorbing_aerosol_index,
                pollution_s5p_CO_column_number_density,
                #pollution_s5p_H2O_column_number_density,
                pollution_s5p_tropospheric_HCHO_column_number_density,
                pollution_s5p_NO2_column_number_density,
                pollution_s5p_O3_column_number_density,
                pollution_s5p_SO2_column_number_density,
                pollution_s5p_CH4_column_volume_mixing_ratio_dry_air)

## Check for and remove NAs
for(var in names(pollution_df)){
  print(var)
  pollution_df[[var]] %>% is.na %>% table() %>% print()
  print(" ")
}

pol_na_df <- pollution_df %>%
  pivot_longer(cols = -c(uid, year)) %>%
  group_by(name) %>%
  dplyr::summarise(N_na = sum(is.na(value)))

pollution_df <- pollution_df %>%
  dplyr::select(-c(pollution_s5p_CH4_column_volume_mixing_ratio_dry_air))

# [Merge] Datasets -------------------------------------------------------------
survey_ancdata_df <- list(survey_df, 
                          fb_df, 
                          #fb_prop_df, 
                          fb_rwi_df,
                          cnn_rgb_df,
                          cnn_ndvi_df,
                          cnn_bu_df,
                          osm_poi_df, 
                          osm_road_df) %>%
  reduce(full_join, by = "uid")

survey_ancdata_df <- list(survey_ancdata_df,
                          gc_df, 
                          wc_df, 
                          viirs_df, 
                          viirs_sd_df,
                          l8_df, 
                          weather,
                          wp_df, 
                          elevslope, 
                          #gbmod_df, 
                          aod_df,
                          pollution_df) %>%
  reduce(full_join, by = c("uid", "year"))

#### LATER DELETE
if(SURVEY_NAME %in% "PAK_CITY_POINTS"){
  survey_ancdata_df <- list(survey_df, 
                            fb_df, 
                            fb_rwi_df) %>%
    reduce(full_join, by = "uid")
  
  survey_ancdata_df <- list(survey_ancdata_df,
                            gc_df, 
                            wc_df,
                            viirs_df) %>%
    reduce(full_join, by = c("uid", "year"))
}

# Dataset specific fixes -------------------------------------------------------
if(SURVEY_NAME %in% "PAK_CITY_POINTS"){
  survey_ancdata_df$iso2 <- "PK" 
}

# [Export] Data ----------------------------------------------------------------
saveRDS(survey_ancdata_df, 
        file.path(data_dir, SURVEY_NAME, "FinalData", "Merged Datasets", "survey_alldata.Rds"))

## Check NAs
nas_df <- survey_ancdata_df %>%
  is.na() %>%
  as.data.frame() %>%
  summarise_all(sum) %>%
  t() %>%
  as.data.frame() %>%
  rownames_to_column(var = "var") %>%
  dplyr::rename(n_na = V1)

nas_df[nas_df$var %>% str_detect("poll")]

nas_df <- nas_df %>%
  dplyr::filter(n_na > 0)


head(survey_ancdata_df)
names(survey_ancdata_df)




