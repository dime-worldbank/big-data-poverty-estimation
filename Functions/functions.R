# Useful functions for project

clean_varnames <- function(df){
  # Clean variable names. Assumes df has a `variable` with the raw names; function
  # creates two variables: `variable_clean` and `variable_cat`. (_cat indicates
  # category).
  
  ## Load Facebook and Globcover parameter name dataframes
  fb_param_df <- readRDS(file.path(data_dir, "Facebook Marketing", "FinalData", "facebook_marketing_parameters_clean.Rds"))
  gc_param_df <- read_csv(file.path(data_dir, "Globcover", "RawData", "gc_classes.csv"))
  
  df <- df %>%
    dplyr::mutate(variable_cat = case_when(
      variable %>% str_detect("^viirs_") ~ "Nighttime Lights",
      variable %>% str_detect("^ntlharmon_") ~ "Nighttime Lights",
      variable %>% str_detect("gc_") ~ "Land Cover",
      variable %>% str_detect("osm_") ~ "OpenStreetMap",
      variable %>% str_detect("l8_") ~ "Daytime Imagery, Average",
      variable %>% str_detect("l7_") ~ "Daytime Imagery, Average",
      # variable %>% str_detect("cnn_s2_rgb_") ~ "Daytime Imagery, CNN: RGB",
      # variable %>% str_detect("cnn_s2_ndvi_") ~ "Daytime Imagery, CNN: NDVI",
      # variable %>% str_detect("cnn_s2_bu_") ~ "Daytime Imagery, CNN: BU",
      
      variable %>% str_detect("cnn_viirs_landsat_rgb_") ~ "Daytime Imagery, CNN",
      variable %>% str_detect("cnn_viirs_landsat_ndvi_") ~ "Daytime Imagery, CNN",
      variable %>% str_detect("cnn_viirs_landsat_bu_") ~ "Daytime Imagery, CNN",
      
      variable %>% str_detect("fb_prop") ~ "Facebook Marketing",
      variable %>% str_detect("worldclim") ~ "WorldClim",
      variable %>% str_detect("worldpop") ~ "World Pop",
      variable %>% str_detect("elevslope") ~ "Elevation/Slope",
      variable %>% str_detect("globalmod") ~ "Global Human Mod Index",
      variable %>% str_detect("pollution_aod") ~ "Pollution - MODIS",
      variable %>% str_detect("pollution_s5p") ~ "Pollution - Sentinel-5P",
      variable %>% str_detect("weather") ~ "Weather",
      TRUE ~ variable
    ))
  
  ### Clean select name for variables
  ## Facebook
  fb_param_clean_df <- fb_param_df %>%
    dplyr::select(param_id, param_category, param_name_simple) %>%
    dplyr::mutate(variable = paste0("fb_prop_estimate_mau_upper_bound_", param_id),
                  variable_clean_fb = paste0(param_category, ": ", param_name_simple)) %>%
    dplyr::select(variable, variable_clean_fb)
  
  ## Globcover
  gc_param_clean_df <- gc_param_df %>%
    dplyr::select(value, label) %>%
    dplyr::rename(variable = value,
                  variable_clean_gc = label) %>%
    dplyr::mutate(variable = paste0("gc_", variable))
  
  df <- df %>%
    left_join(fb_param_clean_df, by = "variable") %>%
    left_join(gc_param_clean_df, by = "variable") %>%
    dplyr::mutate(variable_clean = case_when(
      variable_cat %>% str_detect("Facebook") ~ variable_clean_fb,
      variable_cat %>% str_detect("Land Cover") ~ variable_clean_gc,
      TRUE ~ variable
    )) %>%
    dplyr::select(-c(variable_clean_fb, variable_clean_gc))
  
  #### Manual variable clean
  df <- df %>%
    dplyr::mutate(variable_clean = case_when(
      variable_clean == "viirs_avg_rad" ~ "Nighttime Lights",
      
      variable %>% str_detect("osm_") ~ variable %>%
        str_replace_all("osm_distmeters_poi_", "Distance: ") %>%
        str_replace_all("osm_distmeters_road_", "Distance: ") %>%
        str_replace_all("osm_", "") %>%
        str_replace_all("_", " ") %>%
        str_replace_all("5000m", "") %>%
        str_squish() %>%
        tools::toTitleCase(),
      
      variable_clean %in% "pollution_s5p_absorbing_aerosol_index" ~ "UV Aerosol Index",
      variable_clean %in% "pollution_s5p_CO_column_number_density" ~ "Carbon Monoxide",
      variable_clean %in% "pollution_s5p_H2O_column_number_density" ~ "H20",
      variable_clean %in% "pollution_s5p_tropospheric_HCHO_column_number_density" ~ "Formaldehyde",
      variable_clean %in% "pollution_s5p_NO2_column_number_density" ~ "Nitrogen Dioxide",
      variable_clean %in% "pollution_s5p_O3_column_number_density" ~ "Ozone",
      variable_clean %in% "pollution_s5p_SO2_column_number_density" ~ "Sulphur Dioxide",
      variable_clean %in% "pollution_s5p_CH4_column_volume_mixing_ratio_dry_air" ~ "Methane",
      variable_clean %in% "pollution_aod_047" ~ "AOD: Blue Band",
      variable_clean %in% "pollution_aod_055" ~ "AOD: Green Band",
      
      variable_clean %in% "ntlharmon_avg" ~ "Nighttime Lights (Avg.)",
      variable_clean %in% "ntlharmon_sd"  ~ "Nighttime Lights (Std. Dev.)",
      
      variable_clean %in% "l8_B1" ~ "Landsat Band 1 - Ultra Blue",
      variable_clean %in% "l8_B2" ~ "Landsat Band 2 - Blue",
      variable_clean %in% "l8_B3" ~ "Landsat Band 3 - Green",
      variable_clean %in% "l8_B4" ~ "Landsat Band 4 - Red",
      variable_clean %in% "l8_B5" ~ "Landsat Band 5 - Near Infrared",
      variable_clean %in% "l8_B6" ~ "Landsat Band 6 - Shortwave Infrared 1",
      variable_clean %in% "l8_B7" ~ "Landsat Band 7 - Shortwave Infrared 2",
      variable_clean %in% "l8_B10" ~ "Landsat Band 10 - Brightness Temperature",
      variable_clean %in% "l8_B11" ~ "Landsat Band 11 - Brightness Temperature",
      variable_clean %in% "l8_NDVI" ~ "Landsat NDVI",
      variable_clean %in% "l8_NDBI" ~ "Landsat NDBI",
      variable_clean %in% "l8_BU" ~ "Landsat BU",
      
      variable_clean %in% "l7_B1" ~ "Landsat Band 1 - Blue",
      variable_clean %in% "l7_B2" ~ "Landsat Band 2 - Green",
      variable_clean %in% "l7_B3" ~ "Landsat Band 3 - Red",
      variable_clean %in% "l7_B4" ~ "Landsat Band 4 - Near Infrared",
      variable_clean %in% "l7_B5" ~ "Landsat Band 5 - Shortwave Infrared 1",
      variable_clean %in% "l7_B7" ~ "Landsat Band 7 - Shortwave Infrared 2",
      variable_clean %in% "l7_NDVI" ~ "Landsat NDVI",
      variable_clean %in% "l7_NDBI" ~ "Landsat NDBI",
      variable_clean %in% "l7_BU" ~ "Landsat BU",
      
      variable_clean %in% "globalmod_mean" ~ "Index",
      
      variable_clean %in% "elevslope_elev" ~ "Elevation",
      variable_clean %in% "elevslope_slope" ~ "Slope",
      
      variable_clean %in% "worldpop_2km" ~ "Population",
      variable_clean %in% "worldpop_5km" ~ "Population",
      variable_clean %in% "worldpop_10km" ~ "Population",
      
      variable_clean %in% "worldclim_bio_1" ~ "Annual Mean Temperature",
      variable_clean %in% "worldclim_bio_2" ~ "Mean Diurnal Range",
      variable_clean %in% "worldclim_bio_3" ~ "Isothermality",
      variable_clean %in% "worldclim_bio_4" ~ "Temperature Seasonality",
      variable_clean %in% "worldclim_bio_5" ~ "Max Temperature of Warmest Month",
      variable_clean %in% "worldclim_bio_6" ~ "Min Temperature of Coldest Month",
      variable_clean %in% "worldclim_bio_7" ~ "Temperature Annual Range",
      variable_clean %in% "worldclim_bio_8" ~ "Mean Temperature of Wettest Quarter",
      variable_clean %in% "worldclim_bio_9" ~ "Mean Temperature of Driest Quarter",
      variable_clean %in% "worldclim_bio_10" ~ "Mean Temperature of Warmest Quarter",
      variable_clean %in% "worldclim_bio_11" ~ "Mean Temperature of Coldest Quarter",
      variable_clean %in% "worldclim_bio_12" ~ "Annual Precipitation",
      variable_clean %in% "worldclim_bio_13" ~ "Precipitation of Wettest Month",
      variable_clean %in% "worldclim_bio_14" ~ "Precipitation of Driest Month",
      variable_clean %in% "worldclim_bio_15" ~ "Precipitation Seasonality",
      variable_clean %in% "worldclim_bio_16" ~ "Precipitation of Wettest Quarter",
      variable_clean %in% "worldclim_bio_17" ~ "Precipitation of Driest Quarter",
      variable_clean %in% "worldclim_bio_18" ~ "Precipitation of Warmest Quarter",
      variable_clean %in% "worldclim_bio_19" ~ "Precipitation of Coldest Quarter",
      
      variable_clean %>% str_detect("cnn_viirs_landsat_bu") ~ str_replace_all(variable_clean, 
                                                                              "cnn_viirs_landsat_bu_pc",
                                                                              "BU CNN Feature "),
      
      variable_clean %>% str_detect("cnn_viirs_landsat_rgb") ~ str_replace_all(variable_clean, 
                                                                               "cnn_viirs_landsat_rgb_pc",
                                                                               "RGB CNN Feature "),
      
      variable_clean %>% str_detect("cnn_viirs_landsat_ndvi") ~ str_replace_all(variable_clean, 
                                                                                "cnn_viirs_landsat_ndvi_pc",
                                                                                "NDVI CNN Feature "),
      
      variable_clean %>% str_detect("weather") ~ variable_clean %>%
        str_replace_all("_",
                        " ") %>%
        str_replace_all("weather", "") %>%
        str_replace_all("2m", "") %>%
        str_squish() %>%
        tools::toTitleCase(),
      
      TRUE ~ variable_clean
    ))
  
  df <- df %>%
    dplyr::mutate(variable_clean = case_when(
      variable_clean %in% "Length all" ~ "Length All Roads",
      TRUE ~ variable_clean
    ))
  
  
  # df <- df %>%
  #   dplyr::mutate(variable_clean_with_dataset = case_when(
  #     variable_clean %in% "viirs_avg_rad" ~ "[VIIRS] Nighttime Lights",
  #     variable_clean %in% "gc_190" ~ "[Globcover] Urban Land",
  #     variable_clean %in% "globalmod_mean" ~ "[Global Human Modification Index]",
  #     variable_clean %in% "osm_length_residential_5000m" ~ "[OSM] Residential Road Length",
  #     variable_clean %in% "cnn_s2_rgb_pc11" ~ "[Daytime Imagery, CNN: RGB] Feature 1",
  #     variable_clean %in% "cnn_s2_ndvi_pc7" ~ "[Daytime Imagery, CNN: NDVI] Feature 1",
  #     variable_clean %in% "cnn_s2_bu_pc16"  ~ "[Daytime Imagery, CNN: Built-Up Index] Feature 1",
  #     variable_clean %in% "worldpop_10km" ~ "[World Pop] Population",
  #     variable_clean %in% "pollution_s5p_tropospheric_NO2_column_number_density" ~ 
  #       "[Pollution, Sentinel-5P] Tropospheric NO2",
  #     variable_clean %in% "l8_B2" ~ "[Landsat 8] Blue Surface Reflectance (B2)",
  #     variable_clean %in% "Interests: Restaurants" ~ "[Facebook] Interests: Restaurants",
  #     variable_clean %in% "weather_q4_minimum_2m_air_temperature" ~ "[Weather] Quarter 4 Min. Temperature",
  #     variable_clean %in% "pollution_aod_047" ~ "[Pollution, MODIS] AOD",
  #     variable_clean %in% "worldclim_bio_6" ~ "[WorldClim] Bio 6",
  #     variable_clean %in% "elevslope_slope" ~ "[Elevation/Slope] Slope",
  #     TRUE ~ variable_clean
  #   ))
  
  df <- df %>%
    dplyr::mutate(variable_clean_with_dataset = paste0("[", variable_cat, "] ", variable_clean))
  
  #### Shortened version of variable names
  df <- df %>%
    dplyr::mutate(variable_clean_short = case_when(
      variable_clean %in% "High-end phones: Samsung Galaxy phone S8/S8+/S9/S9+ or Apple iPhone X/XS/XS Max/XR" ~ 
        "High-end phones: Samsung Galaxy phone S8+ or Apple iPhone X+",
      TRUE ~ variable_clean
    ))
  
  return(df)
}



get_lat_lon <- function(number){
  # Transform lat/lon degree/minute/second data from OPM to decimal degrees.
  
  deg = floor(number / 100)
  min = floor(number - (100 * deg))
  sec = 100 * (number - (100 * deg) - min)
  degree = deg + (min / 60) + (sec / 3600)
  
  return(degree)
}

# Function to CLuster Unique Crashes into Crash 
create_clusters <- function(crashes_df,
                            lat_var,
                            lon_var,
                            near_crash_thresh_meters,
                            near_anycrash_in_cluster_thresh_meters){
  
  # DESCRIPTION:
  # Cluster unique crashes into crash clusters. Grab all crashes within 
  # [near_crash_thresh_meters] meters, not allowing the maximum distance of the
  # cluster to grow beyond [near_anycrash_in_cluster_thresh_meters]. Currently
  # assumes WGS84
  
  # PARAMETERS
  # crashes_df: Crash dataframe
  # lat_var: Name of variable indicating latitude
  # lon_var: Name of variable indicating longitude
  # near_crash_thresh_meters: Grab all crashes within this distance.
  # near_anycrash_in_cluster_thresh_meters: Don't allow distance between any
  # points in crash cluster to go beyond this.
  
  crashes_df$longitude <- crashes_df[[lon_var]]
  crashes_df$latitude <- crashes_df[[lat_var]]
  
  # Sory by latitude and longitude
  crashes_df <- crashes_df[order(crashes_df$latitude, crashes_df$longitude),]
  
  # Give cluster ID --------------------------------------------------------------
  crashes_df$id <- 1:nrow(crashes_df)
  crashes_df$cluster_id <- NA
  
  for(i in 1:nrow(crashes_df)){
    if((i %% 500) %in% 0) print(i)
    
    crashes_df_i <- crashes_df[i,]
    within_distance <- sqrt((crashes_df_i$longitude - crashes_df$longitude)^2 + (crashes_df_i$latitude - crashes_df$latitude)^2) <= (near_crash_thresh_meters/1000/111.12)
    
    # If has a cluster ID, consider giving cluster id
    if(!is.na(crashes_df_i$cluster_id)){
      
      # Check if all crashes in cluster are within threshold distance
      current_cluster <- crashes_df[crashes_df$cluster_id %in% crashes_df_i$cluster_id,]
      within_cluster_distance <- sqrt((crashes_df_i$longitude - current_cluster$longitude)^2 + (crashes_df_i$latitude - current_cluster$latitude)^2) <= (near_anycrash_in_cluster_thresh_meters/1000/111.12)
      
      # If a crash is not within thresh distance, give ID as cluster; otherwise, add to cluster
      if(FALSE %in% within_cluster_distance){
        crashes_df$cluster_id[within_distance] <- crashes_df_i$id
      } else{
        crashes_df$cluster_id[within_distance] <- crashes_df_i$cluster_id
      }
      
      # If doesn't have cluster ID, give ID.
    } else{
      crashes_df$cluster_id[within_distance] <- crashes_df_i$id
    }
    
  }
  
  # Create Convex Hull -----------------------------------------------------------
  # Convert to Spatial Points Object
  #coordinates(crashes_df) <- ~longitude+latitude
  #crs(crashes_df) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
  
  # Raster aggregate
  #crashes_df <- gBuffer(crashes_df, byid=T, width=0.000001)
  #crashes_df$N_crashes <- 1
  #crashes_cluster <- raster::aggregate(crashes_df, by="cluster_id", 
  #                                     sums=list(list(sum, 'N_crashes')))
  
  #crashes_cluster_hull <- gConvexHull(crashes_cluster, byid = T)
  #crashes_cluster_hull$id <- 1:length(crashes_cluster_hull)
  #crashes_cluster_hull$N_crashes <- crashes_cluster$N_crashes
  
  return(crashes_df)
}







