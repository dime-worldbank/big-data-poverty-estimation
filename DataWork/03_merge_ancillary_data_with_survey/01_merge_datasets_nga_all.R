# Merge Data with Survey Data

# [Load] Survey data -----------------------------------------------------------
INV_DATA_DIR <- file.path(data_dir, "DHS_nga_policy_experiment", "FinalData", "Individual Datasets")

survey_df <- readRDS(file.path(INV_DATA_DIR, "survey_socioeconomic.Rds"))

#INV_DATA_DIR <- file.path(data_dir, "DHS_OLD", "FinalData", "Individual Datasets")

# [Load] CNN Features ----------------------------------------------------------
## Landsat
cnn_landsat_rgb_df <- readRDS(file.path(INV_DATA_DIR, "cnn_features", 
                                        "cnn_features_landsat_ntlharmon_underiaTrue_b_rgb_pca.Rds"))

cnn_landsat_ndvi_df <- readRDS(file.path(INV_DATA_DIR, "cnn_features", 
                                         "cnn_features_landsat_ntlharmon_underiaTrue_b_ndvi_pca.Rds"))

cnn_landsat_bu_df <- readRDS(file.path(INV_DATA_DIR, "cnn_features", 
                                       "cnn_features_landsat_ntlharmon_underiaTrue_b_bu_pca.Rds"))

# [Load] Globcover -------------------------------------------------------------
gc_df <- readRDS(file.path(INV_DATA_DIR, "globcover.Rds"))

# [Load] Harmonized NTL --------------------------------------------------------
ntlharmon_df <- readRDS(file.path(INV_DATA_DIR, paste0("ntl_harmonized_",BUFFER_SATELLITE,".Rds")))



# [Load] Satellite data from GEE -----------------------------------------------
#file.path(INV_DATA_DIR, "satellite_data_from_gee") %>% list.files()

##### ** Landsat 7 #####
l7_df <- readRDS(file.path(INV_DATA_DIR, "satellite_data_from_gee", 
                           paste0("l7_ubuff",BUFFER_SATELLITE,"_rbuff",BUFFER_SATELLITE,".Rds")))

l7_df <- l7_df %>%
  rename_at(vars(-uid, -year), ~ paste0("l7_", .))

##### ** Landsat 7 SD Time #####
l7_sdtime_df <- readRDS(file.path(INV_DATA_DIR, "satellite_data_from_gee", 
                                  paste0("l7_sdtime_ubuff",BUFFER_SATELLITE,"_rbuff",BUFFER_SATELLITE,".Rds")))

l7_sdtime_df <- l7_sdtime_df %>%
  rename_at(vars(-uid, -year), ~ paste0("l7_", .) %>% str_replace_all("stdDev", "sdtime") %>% tolower())

##### ** Landsat 7 SD Space #####
l7_sdspace_df <- readRDS(file.path(INV_DATA_DIR, "satellite_data_from_gee", 
                                   paste0("l7_sdspace_ubuff",BUFFER_SATELLITE,"_rbuff",BUFFER_SATELLITE,".Rds")))

l7_sdspace_df <- l7_sdspace_df %>%
  rename_at(vars(-uid, -year), ~ paste0("l7_", ., "_sdspace") %>% tolower())

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

# TODO: Don't have all data here!
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

# [Merge] Datasets -------------------------------------------------------------
survey_ancdata_df <- list(survey_df, 
                          cnn_landsat_rgb_df,
                          cnn_landsat_ndvi_df,
                          cnn_landsat_bu_df) %>%
  reduce(full_join, by = "uid") %>%
  dplyr::filter(!is.na(country_code))

survey_ancdata_df <- list(survey_ancdata_df,
                          gc_df, 
                          ntlharmon_df,
                          l7_df, ## FIX
                          l7_sdtime_df,
                          l7_sdspace_df,
                          weather,
                          aod_df) %>%
  reduce(full_join, by = c("uid", "year")) %>%
  dplyr::filter(!is.na(country_code))

# [Export] Data ----------------------------------------------------------------
saveRDS(survey_ancdata_df, 
        file.path(data_dir, SURVEY_NAME, "FinalData", "Merged Datasets", "survey_alldata.Rds"))

