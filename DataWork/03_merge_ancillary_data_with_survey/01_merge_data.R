# Merge Data with Survey Data

# [Load] Survey data -----------------------------------------------------------
INV_DATA_DIR <- file.path(data_dir, "DHS", "FinalData", "Individual Datasets")

survey_df <- readRDS(file.path(INV_DATA_DIR, "survey_socioeconomic.Rds"))

#INV_DATA_DIR <- file.path(data_dir, "DHS_OLD", "FinalData", "Individual Datasets")

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

# [Load] MOSAIKS ---------------------------------------------------------------
mosaik_df <- readRDS(file.path(data_dir, SURVEY_NAME, "FinalData", 
                               "Individual Datasets", "mosaik_pca.Rds"))

# [Load] CNN Features ----------------------------------------------------------
## Landsat
cnn_landsat_rgb_df <- readRDS(file.path(INV_DATA_DIR, "cnn_features", 
                                        "cnn_features_landsat_ntlharmon_underiaTrue_b_rgb_pca.Rds"))

cnn_landsat_ndvi_df <- readRDS(file.path(INV_DATA_DIR, "cnn_features", 
                                         "cnn_features_landsat_ntlharmon_underiaTrue_b_ndvi_pca.Rds"))

cnn_landsat_bu_df <- readRDS(file.path(INV_DATA_DIR, "cnn_features", 
                                       "cnn_features_landsat_ntlharmon_underiaTrue_b_bu_pca.Rds"))

## Sentinel
cnn_sentinel_rgb_df <- readRDS(file.path(INV_DATA_DIR, "cnn_features", 
                                         "cnn_features_s2_viirs_underiaTrue_b_rgb_pca.Rds"))

cnn_sentinel_ndvi_df <- readRDS(file.path(INV_DATA_DIR, "cnn_features", 
                                          "cnn_features_s2_viirs_underiaTrue_b_ndvi_pca.Rds"))

cnn_sentinel_bu_df <- readRDS(file.path(INV_DATA_DIR, "cnn_features", 
                                        "cnn_features_s2_viirs_underiaTrue_b_bu_pca.Rds"))

# [Load] Globcover -------------------------------------------------------------
gc_df <- readRDS(file.path(INV_DATA_DIR, "globcover.Rds"))

# [Load] WorldClim -------------------------------------------------------------
wc_df <- readRDS(file.path(INV_DATA_DIR, "worldclim.Rds"))

# [Load] Harmonized NTL --------------------------------------------------------
ntlharmon_df <- readRDS(file.path(INV_DATA_DIR, paste0("ntl_harmonized_",BUFFER_SATELLITE,".Rds")))

# [Load] Sentinel 5P -----------------------------------------------------------
s5p_df <- readRDS(file.path(INV_DATA_DIR, "sentinel5p.Rds"))

s5p_df <- s5p_df %>% 
  rename_with(~paste0("pollution_s5p_", .), -c(uid))

s5p_df <- s5p_df %>%
  dplyr::select(uid,
                pollution_s5p_absorbing_aerosol_index,
                pollution_s5p_CO_column_number_density,
                #pollution_s5p_H2O_column_number_density,
                pollution_s5p_tropospheric_HCHO_column_number_density,
                pollution_s5p_NO2_column_number_density,
                pollution_s5p_O3_column_number_density,
                pollution_s5p_SO2_column_number_density)


## OLD
if(F){
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
  
  pol_na_df <- s5p_df %>%
    pivot_longer(cols = -c(uid)) %>%
    group_by(name) %>%
    dplyr::summarise(N_na = sum(is.na(value)))
}

# [Load] Satellite data from GEE -----------------------------------------------
#file.path(INV_DATA_DIR, "satellite_data_from_gee") %>% list.files()

##### ** VIIRS, Mean #####
viirs_df <- readRDS(file.path(INV_DATA_DIR, "satellite_data_from_gee", 
                              paste0("viirs_",
                                     BUFFER_SATELLITE,
                                     "_ubuff",
                                     BUFFER_SATELLITE,
                                     "_rbuff",
                                     BUFFER_SATELLITE,".Rds")))

viirs_df <- viirs_df %>%
  rename_at(vars(-uid, -year), ~ paste0("viirs_", .))

##### ** VIIRS, SD Time #####
viirs_sdtime_df <- readRDS(file.path(INV_DATA_DIR, "satellite_data_from_gee", 
                                     paste0("viirs_sdtime_",
                                            BUFFER_SATELLITE,
                                            "_ubuff",
                                            BUFFER_SATELLITE,
                                            "_rbuff",
                                            BUFFER_SATELLITE,".Rds")))

viirs_sdtime_df <- viirs_sdtime_df %>%
  dplyr::rename(viirs_avg_rad_sdtime = avg_rad_stdDev)

##### ** VIIRS, SD Space #####
viirs_sdspace_df <- readRDS(file.path(INV_DATA_DIR, "satellite_data_from_gee", 
                                      paste0("viirs_sdspace_",
                                             BUFFER_SATELLITE,
                                             "_ubuff",
                                             BUFFER_SATELLITE,
                                             "_rbuff",
                                             BUFFER_SATELLITE,".Rds")))

viirs_sdspace_df <- viirs_sdspace_df %>%
  dplyr::rename(viirs_avg_rad_sdspace = avg_rad)

##### ** Landsat 8 #####
l8_df <- readRDS(file.path(INV_DATA_DIR, "satellite_data_from_gee", 
                           paste0("l8_ubuff",BUFFER_SATELLITE,"_rbuff",BUFFER_SATELLITE,".Rds")))

l8_df <- l8_df %>%
  rename_at(vars(-uid, -year), ~ paste0("l8_", .))

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

##### ** Sentinel 1 - SAR #####
## Grab list of s1 files
s1_files <- file.path(INV_DATA_DIR, "satellite_data_from_gee") %>% 
  list.files() %>%
  str_subset("s1_sar_")

## Load, rename using username, then merge
s1_df <- lapply(s1_files, function(file_i){
  
  s1_i_df <- readRDS(file.path(INV_DATA_DIR, "satellite_data_from_gee", file_i))
  
  var_name <- file_i %>% str_replace_all("_ubuff.*", "")
  
  names(s1_i_df)[names(s1_i_df) %in% "mean"] <- var_name
  
  return(s1_i_df)
}) %>%
  reduce(left_join, by = c("uid", "year"))

## Take maximum across descending and ascending
s1_df <- s1_df %>%
  dplyr::mutate(s1_sar_vv_mean   = pmax(s1_sar_vv_desc_mean,   s1_sar_vv_asc_mean, na.rm = T),
                s1_sar_vh_mean   = pmax(s1_sar_vh_desc_mean,   s1_sar_vh_asc_mean, na.rm = T),
                s1_sar_vdiv_mean = pmax(s1_sar_vdiv_desc_mean, s1_sar_vdiv_asc_mean, na.rm = T),
                
                s1_sar_vv_stddev   = pmax(s1_sar_vv_desc_stddev,   s1_sar_vv_asc_stddev, na.rm = T),
                s1_sar_vh_stddev   = pmax(s1_sar_vh_desc_stddev,   s1_sar_vh_asc_stddev, na.rm = T),
                s1_sar_vdiv_stddev = pmax(s1_sar_vdiv_desc_stddev, s1_sar_vdiv_asc_stddev, na.rm = T)) %>%
  dplyr::select(uid, 
                s1_sar_vv_mean,   s1_sar_vh_mean,   s1_sar_vdiv_mean)
# These are NA
#  s1_sar_vv_stddev, s1_sar_vh_stddev, s1_sar_vdiv_stddev

# [Merge] Datasets -------------------------------------------------------------
survey_ancdata_df <- list(survey_df, 
                          fb_df, 
                          #fb_prop_df, 
                          s5p_df,
                          fb_rwi_df,
                          cnn_landsat_rgb_df,
                          cnn_landsat_ndvi_df,
                          cnn_landsat_bu_df,
                          cnn_sentinel_rgb_df,
                          cnn_sentinel_ndvi_df,
                          cnn_sentinel_bu_df,
                          osm_poi_df, 
                          osm_road_df,
                          mosaik_df,
                          s1_df) %>%
  reduce(full_join, by = "uid") %>%
  dplyr::filter(!is.na(country_code))

# dff <- pollution_df #???
# paste(dff$uid, dff$year) %>% table %>% table

survey_ancdata_df <- list(survey_ancdata_df,
                          gc_df, 
                          wc_df, 
                          viirs_df, 
                          viirs_sdtime_df,
                          viirs_sdspace_df,
                          ntlharmon_df,
                          l8_df, 
                          l7_df, ## FIX
                          l7_sdtime_df,
                          l7_sdspace_df,
                          weather,
                          wp_df, 
                          elevslope, 
                          aod_df) %>%
  reduce(full_join, by = c("uid", "year")) %>%
  dplyr::filter(!is.na(country_code))

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

