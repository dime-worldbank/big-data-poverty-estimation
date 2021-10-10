# Merge Data with Survey Data

INV_DATA_DIR <- file.path(data_dir, SURVEY_NAME, "FinalData", "Individual Datasets")

# [Load] Survey data -----------------------------------------------------------
survey_df <- readRDS(file.path(INV_DATA_DIR, "survey_socioeconomic.Rds"))

# [Load] Facebook --------------------------------------------------------------
fb_df <- readRDS(file.path(INV_DATA_DIR, "facebook_marketing_dau_mau.Rds"))
fb_prop_df <- readRDS(file.path(INV_DATA_DIR, "facebook_marketing_dau_mau_prop.Rds"))

fb_df$estimate_dau_NA <- NULL
fb_df$estimate_mau_NA <- NULL

fb_prop_df$fb_prop_estimate_dau_NA <- NULL
fb_prop_df$fb_prop_estimate_mau_NA <- NULL

fb_df <- fb_df %>%
  rename_at(vars(-uid), ~ paste0("fb_", .))

fb_prop_df <- fb_prop_df %>%
  rename_at(vars(-uid), ~ paste0("fb_prop_", .))

# [Load] OSM -------------------------------------------------------------------
osm_poi_df  <- readRDS(file.path(INV_DATA_DIR, "osm_poi.Rds"))
osm_road_df <- readRDS(file.path(INV_DATA_DIR, "osm_road.Rds"))

# [Load] CNN Features ----------------------------------------------------------
#gc_df <- readRDS(file.path(INV_DATA_DIR, "globcover.Rds"))

# [Load] Globcover -------------------------------------------------------------
gc_df <- readRDS(file.path(INV_DATA_DIR, "globcover.Rds"))

# [Load] WorldClim -------------------------------------------------------------
wc_df <- readRDS(file.path(INV_DATA_DIR, "worldclim.Rds"))

# [Load] Satellite data from GEE -----------------------------------------------
#file.path(INV_DATA_DIR, "satellite_data_from_gee") %>% list.files()

##### ** VIIRS #####
viirs_df <- readRDS(file.path(INV_DATA_DIR, "satellite_data_from_gee", 
                              "viirs_ubuff2500_rbuff2500.Rds"))

viirs_df <- viirs_df %>%
  rename_at(vars(-uid, -year), ~ paste0("viirs_", .))

##### ** Landsat 8 #####
l8_df <- readRDS(file.path(INV_DATA_DIR, "satellite_data_from_gee", 
                           "l8_ubuff2500_rbuff2500.Rds"))

l8_df <- l8_df %>%
  rename_at(vars(-uid, -year), ~ paste0("l8_", .))

##### ** World pop #####
wp10km_df <- readRDS(file.path(INV_DATA_DIR, "satellite_data_from_gee", 
                               "worldpop_ubuff10000_rbuff10000.Rds"))

wp10km_df <- wp10km_df %>%
  dplyr::rename(worldpop_10km = sum)

##### ** Elevation/Slope #####
elev_df <- readRDS(file.path(INV_DATA_DIR, "satellite_data_from_gee", 
                             "elevation_ubuff5000_rbuff5000.Rds")) %>%
  dplyr::rename(elev = mean)

slope_df <- readRDS(file.path(INV_DATA_DIR, "satellite_data_from_gee", 
                              "slope_ubuff5000_rbuff5000.Rds")) %>%
  dplyr::rename(slope = mean)

elevslope <- full_join(elev_df, slope_df, by = c("uid", "year")) %>%
  rename_at(vars(-uid, -year), ~ paste0("elevslope_", .))

##### ** Global human modification index #####
gbmod_df <- readRDS(file.path(INV_DATA_DIR, "satellite_data_from_gee", 
                              "GlobalHumanModification_ubuff10000_rbuff10000.Rds"))

gbmod_df <- gbmod_df %>%
  dplyr::rename(globalmod_mean = mean)

##### ** Weather #####
weather_q1 <- readRDS(file.path(INV_DATA_DIR, "satellite_data_from_gee", 
                                "ecmwf_weather_q1_ubuff10000_rbuff10000.Rds")) %>%
  rename_at(vars(-uid, -year), ~ paste0("weather_q1_", .))

weather_q2 <- readRDS(file.path(INV_DATA_DIR, "satellite_data_from_gee", 
                                "ecmwf_weather_q2_ubuff10000_rbuff10000.Rds")) %>%
  rename_at(vars(-uid, -year), ~ paste0("weather_q2_", .))

weather_q3 <- readRDS(file.path(INV_DATA_DIR, "satellite_data_from_gee", 
                                "ecmwf_weather_q3_ubuff10000_rbuff10000.Rds")) %>%
  rename_at(vars(-uid, -year), ~ paste0("weather_q3_", .))

weather_q4 <- readRDS(file.path(INV_DATA_DIR, "satellite_data_from_gee", 
                                "ecmwf_weather_q4_ubuff10000_rbuff10000.Rds")) %>%
  rename_at(vars(-uid, -year), ~ paste0("weather_q4_", .))

weather_annual <- readRDS(file.path(INV_DATA_DIR, "satellite_data_from_gee", 
                             "ecmwf_weather_ubuff10000_rbuff10000.Rds")) %>%
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
  str_subset("2500") %>%
  lapply(readRDS) %>%
  reduce(full_join, by = c("uid", "year")) %>%
  rename_at(vars(-uid, -year), ~ paste0("pollution_", .))

## Check for and remove NAs
for(var in names(pollution_df)){
  print(var)
  pollution_df[[var]] %>% is.na %>% table() %>% print()
  print(" ")
}

pollution_df <- pollution_df %>%
  dplyr::select(-c(pollution_CH4_column_volume_mixing_ratio_dry_air,
                   pollution_CO_column_number_density,
                   pollution_H2O_column_number_density,
                   pollution_absorbing_aerosol_index))

# [Merge] Datasets -------------------------------------------------------------
survey_ancdata_df <- list(survey_df, 
                          fb_df, 
                          fb_prop_df, 
                          osm_poi_df, 
                          osm_road_df) %>%
  reduce(full_join, by = "uid")

survey_ancdata_df <- list(survey_ancdata_df,
                          gc_df, 
                          wc_df, 
                          viirs_df, 
                          l8_df, 
                          weather,
                          wp10km_df, 
                          elevslope, 
                          gbmod_df, 
                          pollution_df) %>%
  reduce(full_join, by = c("uid", "year"))

if(F){
  survey_ancdata_df <- list(survey_df, 
                            gc_df, 
                            osm_poi_df, 
                            osm_road_df) %>%
    reduce(full_join, by = "uid")
  
  survey_ancdata_df <- list(survey_ancdata_df, 
                            viirs_df,
                            l8_df,
                            wp10km_df,
                            elevslope,
                            gbmod_df,
                            pollution_df) %>%
    reduce(full_join, by = c("uid", "year"))
}

# [Export] Data ----------------------------------------------------------------
saveRDS(survey_ancdata_df, 
        file.path(data_dir, SURVEY_NAME, "FinalData", "Merged Datasets", "survey_alldata.Rds"))



