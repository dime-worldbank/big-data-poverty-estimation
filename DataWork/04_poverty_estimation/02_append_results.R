# Append Results

for(model_type in c("xgboost", "glmnet", "svm")){
  
  # Append data ------------------------------------------------------------------
  df <- file.path(data_dir, "DHS", "FinalData", "pov_estimation_results",
                  "prediction") %>% 
    list.files(full.names = T, pattern = "*.Rds") %>%
    str_subset("prediction_") %>%
    str_subset("_levels_|_changes_") %>%
    str_subset(model_type) %>%
    map_df(readRDS) %>%
    dplyr::filter(!is.na(prediction),
                  !is.na(target_var))
  
  #### Aggregate data
  sum_df <- df %>%
    group_by(country_code, country_name, estimation_type, feature_type, target_var_dep, level_change) %>%
    dplyr::summarise(r2 = cor(target_var, prediction)^2,
                     R2 = R2(pred = prediction, obs = target_var, form = "traditional"),
                     n = n()) %>%
    ungroup() %>%
    dplyr::filter(!is.na(r2))
  
  # Make best estimation type ----------------------------------------------------
  #### Determine best est type
  best_sum_df <- sum_df %>%
    dplyr::filter(feature_type %in% c("all", "all_changes")) %>%
    arrange(-r2) %>%
    group_by(country_code, level_change) %>%
    slice_max(r2, n = 1) %>%
    ungroup() %>%
    dplyr::rename(estimation_type_orig = estimation_type) %>%
    dplyr::mutate(estimation_type = "best",
                  est_cnty = paste(estimation_type_orig, country_code, level_change))
  
  best_df <- df %>%
    dplyr::filter(feature_type %in% c("all", "all_changes")) %>%
    dplyr::mutate(est_cnty = paste(estimation_type, country_code, level_change))
  
  best_df <- best_df[best_df$est_cnty %in% best_sum_df$est_cnty,] %>%
    dplyr::rename(estimation_type_orig = estimation_type) %>%
    dplyr::mutate(estimation_type = "best")
  
  #### Add best estimation type to dataframes
  df     <- bind_rows(df, best_df)
  sum_df <- bind_rows(sum_df, best_sum_df)
  
  # Cleanup ----------------------------------------------------------------------
  #### Clean estimation type
  sum_df <- sum_df %>%
    dplyr::mutate(estimation_type_clean = case_when(
      estimation_type == "best" ~ "Using Best Training Sample\nType for Each Country",
      estimation_type == "continent" ~ "Same Continent",
      estimation_type == "global_country_pred" ~ "Global",
      estimation_type == "other_continent" ~ "Other Continents",
      estimation_type == "within_country_cv" ~ "Within Country"
    ))
  
  df <- df %>%
    dplyr::mutate(estimation_type_clean = case_when(
      estimation_type == "best" ~ "Using Best Training Sample\nType for Each Country",
      estimation_type == "continent" ~ "Same Continent",
      estimation_type == "global_country_pred" ~ "Global",
      estimation_type == "other_continent" ~ "Other Continents",
      estimation_type == "within_country_cv" ~ "Within Country"
    ))
  
  #### Clean feature type
  clean_feature_type <- function(df){
    
    df %>%
      dplyr::mutate(feature_type_clean = case_when(
        feature_type == "all" ~ "All Features",
        feature_type == "all_changes" ~ "All Features",
        feature_type == "cnn_s2_bu" ~ "CNN: Built-Up Index",
        feature_type == "cnn_s2_ndvi" ~ "CNN: NDVI",
        feature_type == "cnn_s2_rgb" ~ "CNN: RGB",
        
        feature_type == "cnn_viirs_landsat" ~ "CNN: All Variables",
        feature_type == "cnn_viirs_landsat_bu" ~ "CNN: Built-Up Index",
        feature_type == "cnn_viirs_landsat_ndvi" ~ "CNN: NDVI",
        feature_type == "cnn_viirs_landsat_rgb" ~ "CNN: RGB",
        
        feature_type == "cnn_ntlharmon_landsat" ~ "CNN: All Variables",
        feature_type == "cnn_ntlharmon_landsat_bu" ~ "CNN: Built-Up Index",
        feature_type == "cnn_ntlharmon_landsat_ndvi" ~ "CNN: NDVI",
        feature_type == "cnn_ntlharmon_landsat_rgb" ~ "CNN: RGB",
        
        feature_type == "cnn_viirs_s2" ~ "CNN: All Variables",
        feature_type == "cnn_viirs_s2_bu" ~ "CNN: Built-Up Index",
        feature_type == "cnn_viirs_s2_ndvi" ~ "CNN: NDVI",
        feature_type == "cnn_viirs_s2_rgb" ~ "CNN: RGB",
        
        feature_type == "fb_prop" ~ "Facebook: Proportion",
        feature_type == "fb" ~ "Facebook",
        feature_type == "satellites" ~ "Day/Night Satellites",
        feature_type == "viirs" ~ "Nighttime Lights",
        feature_type == "ntlharmon" ~ "Nighttime Lights",
        feature_type == "landcover" ~ "Landcover",
        feature_type == "weatherclimate" ~ "Weather/Climate",
        feature_type == "weather" ~ "Weather",
        feature_type == "gc" ~ "Landcover - GlobCover",
        feature_type == "l8" ~ "Daytime Imagery",
        feature_type == "l7" ~ "Daytime Imagery",
        feature_type == "osm" ~ "OpenStreetMap",
        feature_type == "pollution" ~ "Pollution",
        feature_type == "pollution_aod" ~ "Pollution",
        feature_type == "satellites_changes" ~ "Day/Night Satellites",
        feature_type == "s1_sar" ~ "SAR",
        feature_type == "mosaik" ~ "MOSAIKS",
        TRUE ~ feature_type
      ))
    
  }
  
  df     <- clean_feature_type(df)
  sum_df <- clean_feature_type(sum_df)
  
  # Merge with select survey/other variables -------------------------------------
  #### Load datasets
  wdi_df <- readRDS(file.path(data_dir, "WDI", "FinalData", "wdi.Rds"))
  
  fb_wide_df <- readRDS(file.path(fb_marketing_dir,  "FinalData", "country_level_mau", 
                                  "Individual Datasets",
                                  "country_level_mau.Rds"))
  
  survey_df <- readRDS(file.path(data_dir, SURVEY_NAME, "FinalData", "Merged Datasets", 
                                 "survey_alldata_clean.Rds"))
  
  survey_changes_df <- readRDS(file.path(data_dir, SURVEY_NAME, "FinalData", "Merged Datasets", 
                                         "survey_alldata_clean_changes_cluster.Rds"))
  
  #### Prep data for merging
  wdi_df <- wdi_df %>%
    dplyr::select(-c(iso3c, country, year, capital, longitude, latitude))
  
  fb_wide_df <- fb_wide_df %>%
    dplyr::rename(iso2 = country_iso2)
  
  survey_sum_df <- survey_df %>%
    dplyr::filter(most_recent_survey %in% T) %>%
    group_by(country_code, country_name, continent_adj, iso2) %>%
    dplyr::summarise(pca_allvars_sd = sd(pca_allvars),
                     pca_allvars_mean = mean(pca_allvars),
                     pca_allvars_mr_sd = sd(pca_allvars_mr),
                     pca_allvars_mr_mean = mean(pca_allvars_mr),
                     
                     pca_allvars_mr_hh_stddev = mean(pca_allvars_mr_stddev),
                     
                     ntlharmon_avg_sd = sd(ntlharmon_avg),
                     viirs_avg_rad_log_sd = sd(log(viirs_avg_rad+1)),
                     
                     ntlharmon_avg = mean(ntlharmon_avg),
                     viirs_avg_rad_sd = sd(viirs_avg_rad),
                     viirs_avg_rad = mean(viirs_avg_rad),
                     viirs_avg_rad_sdspace = mean(viirs_avg_rad_sdspace),
                     prop_urban = mean(urban_rural %in% "U"),
                     survey_year = year[1],
                     N_dhs_obs = n()) %>%
    ungroup() %>%
    dplyr::rename(country = country_code)
  
  survey_changes_sum_df <- survey_changes_df %>%
    group_by(iso2, year_diff) %>%
    dplyr::summarise(pca_allvars_sd_change = sd(pca_allvars),
                     ntlharmon_avg_sd_change = sd(ntlharmon_avg),
                     ntlharmon_avg_change = mean(ntlharmon_avg),
                     pca_allvars_avg_change = mean(pca_allvars),
                     pca_allvars_p75_change = pca_allvars %>% 
                       quantile(probs = 0.75) %>% 
                       as.numeric()) %>%
    ungroup()
  
  #### Merge
  otherdata_df <- survey_sum_df %>%
    left_join(wdi_df, by = "iso2") %>%
    left_join(fb_wide_df, by = "iso2") %>%
    left_join(survey_changes_sum_df, by = "iso2")  
  
  #### Construct variables 
  otherdata_df <- otherdata_df %>%
    ungroup() %>%
    dplyr::mutate(prop_pop_on_fb = estimate_mau_1 / wdi_population,
                  income = income %>% as.character() %>% as.factor() %>%
                    relevel(ref = "Low income"))
  
  #### Merge
  sum_df <- sum_df %>%
    left_join(otherdata_df, by = "country_name")
  
  # Export -----------------------------------------------------------------------
  if(model_type == "xgboost") suffix <- ""
  if(model_type == "glmnet")  suffix <- "_glmnet"
  if(model_type == "svm")     suffix <- "_svm"
  
  df$model_type     <- model_type
  sum_df$model_type <- model_type
  
  saveRDS(df, 
          file.path(data_dir, SURVEY_NAME, "FinalData", "pov_estimation_results",
                    paste0("predictions_appended",suffix,".Rds")))
  
  saveRDS(sum_df, 
          file.path(data_dir, SURVEY_NAME, "FinalData", "pov_estimation_results",
                    paste0("accuracy_appended",suffix,".Rds")))
}

