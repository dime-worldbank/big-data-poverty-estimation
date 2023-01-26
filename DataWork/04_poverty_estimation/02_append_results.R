# Append Poverty Estimation Results

# Load/append data -------------------------------------------------------------
acc_df <- file.path(data_dir, SURVEY_NAME, "FinalData", "pov_estimation_results", 
                    "accuracy") %>%
  list.files(pattern = "*.Rds",
             full.names = T) %>%
  map_df(readRDS)

## DELETE LATER
# acc_levels_df <- acc_df %>%
#   dplyr::filter(level_change %in% "levels",
#                 xg_max.depth %in% 5,
#                 xg_eta %in% 0.3,
#                 xg_nthread %in% 4,
#                 xg_nrounds %in% c(50, 100),
#                 xg_subsample %in% 0.3,
#                 xg_objective %in% "reg:squarederror",
#                 xg_min_child_weight %in% 1)
# 
# acc_changes_df <- acc_df %>%
#   dplyr::filter(level_change %in% "changes")
# 
# acc_df <- bind_rows(acc_levels_df,
#                     acc_changes_df)

# Clean ------------------------------------------------------------------------
acc_df <- acc_df %>%
  # For continent, predict across all countries; but we care about aggregating 
  # to countries
  dplyr::mutate(cor_country = case_when(
    estimation_type == "continent" ~ cor_fold,
    TRUE ~ cor_all
  )) %>%
  dplyr::mutate(country_code = country_code %>% as.character()) %>%
  
  # Retains original value of country; for "continent" (predict on other continents), 
  # is "all" instead of country code
  dplyr::mutate(country_predict_group = country) %>%
  
  # For continent, originally:
  # --"country" --> "all"
  # --"country_code" -->  individual country code
  # Change so "country" contains the individual country code, as interested in
  # how well the continent predicts in individual countries.
  dplyr::mutate(country = case_when(
    estimation_type %in% "continent" ~ country_code,
    TRUE ~ country
  )) %>%
  dplyr::mutate(estimation_type_clean = case_when(
    estimation_type %>% str_detect("continent_") & estimation_type %>% str_detect("country_pred") ~ "Same Continent",
    estimation_type %in% "global_country_pred" ~ "Global",
    estimation_type %in% "within_country_cv" ~ "Within Country",
    estimation_type %in% "continent" ~ "Other Continents"
  )) %>%
  dplyr::mutate(estimation_type = case_when(
    estimation_type %>% str_detect("continent_") & estimation_type %>% str_detect("country_pred") ~ "same_continent",
    estimation_type %in% "continent" ~ "other_continents",
    TRUE ~ estimation_type
  )) %>%
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
    #feature_type == "" ~ "",
    TRUE ~ feature_type
  )) %>%
  dplyr::mutate(target_var_clean = case_when(
    target_var == "pca_allvars" ~ "Asset Index - All Periods",
    target_var == "pca_allvars_mr" ~ "Asset Index",
    target_var == "pca_nonphysicalvars_mr" ~ "Asset Index: Non-Physical Assets",
    target_var == "pca_physicalvars_mr" ~ "Asset Index: Physical Assets",
    target_var == "wealth_index_score" ~ "DHS Wealth Index",
    TRUE ~ target_var
  )) %>%
  dplyr::mutate(target_var_clean = factor(target_var_clean,
                                          levels = c("DHS Wealth Index",
                                                     "Asset Index - All Periods",
                                                     "Asset Index",
                                                     "Asset Index: Physical Assets",
                                                     "Asset Index: Non-Physical Assets")))

# Aggregate Across Folds -------------------------------------------------------
# Notes:
# - cor_fold only different than cor_country for within_country estimation
# - cor_all only different than cor_country for other continents estimation

acc_all_df <- acc_df %>%
  group_by(level_change,
           n,
           estimation_type, estimation_type_clean,
           feature_type, feature_type_clean,
           target_var, target_var_clean, 
           xg_max.depth, xg_eta, xg_nthread, xg_nrounds, xg_subsample, xg_objective, xg_min_child_weight,
           country_predict_group, country) %>%
  dplyr::summarise(N = sum(N_fold),
                   cor = cor_country[1]) %>% # This repeats across folds
  dplyr::mutate(r2 = cor^2)

# Add rows for best estimation type WITHIN each set of parameters --------------
# acc_all_best_df <- acc_all_df %>%
#   group_by(country,
#            n,
#            level_change,
#            target_var, target_var_clean,
#            feature_type, feature_type_clean,
#            xg_max.depth, xg_eta, xg_nthread, xg_nrounds, xg_subsample, xg_objective, xg_min_child_weight) %>%
#   slice_max(order_by = r2, n = 1) %>%
#   mutate(estimation_type_clean = "Using Best Training Sample\nType for Each Country",
#          estimation_type = "best") %>%
#   mutate(est_cat = "Best")

acc_all_best_df <- acc_all_df %>%
  group_by(country,
           N,
           level_change,
           feature_type, feature_type_clean,
           target_var, target_var_clean) %>%
  slice_max(order_by = cor, n = 1) %>%
  mutate(estimation_type_clean = "Using Best Training Sample\nType for Each Country",
         estimation_type = "best",
         est_cat = "Best") #%>%
#dplyr::rename(n = N) # TODO: Check n?

acc_all_df <- bind_rows(
  acc_all_df,
  acc_all_best_df
)

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
acc_all_df <- acc_all_df %>%
  left_join(otherdata_df, by = "country")

# Add variable for parameter set -----------------------------------------------
acc_all_df <- acc_all_df %>%
  dplyr::mutate(xg_param_set = paste(xg_max.depth,
                                     xg_eta,
                                     xg_nthread,
                                     xg_nrounds,
                                     xg_subsample,
                                     xg_objective,
                                     xg_min_child_weight,sep = "_") %>%
                  str_replace_all("[:punct:]", "_"))

acc_all_df <- acc_all_df %>%
  dplyr::mutate(model_param = paste(continent_adj,
                                    estimation_type,
                                    target_var,
                                    feature_type,
                                    xg_max.depth,
                                    xg_eta,
                                    xg_nthread,
                                    xg_nrounds,
                                    xg_subsample,
                                    xg_objective,
                                    xg_min_child_weight,
                                    sep = "_")) 

acc_all_df <- acc_all_df %>%
  dplyr::mutate(country_model_param = paste(country,
                                            estimation_type,
                                            target_var,
                                            feature_type,
                                            xg_max.depth,
                                            xg_eta,
                                            xg_nthread,
                                            xg_nrounds,
                                            xg_subsample,
                                            xg_objective,
                                            xg_min_child_weight,
                                            sep = "_")) 


# acc_all_best_param_df <- acc_all_best_param_df %>%
#   left_join(otherdata_df, by = "country")

# Add model parameter variable -------------------------------------------------
# acc_all_df <- acc_all_df %>%
#   dplyr::mutate(model_param = paste(continent_adj,
#                                     estimation_type,
#                                     target_var,
#                                     feature_type,
#                                     xg_max.depth,
#                                     xg_eta,
#                                     xg_nthread,
#                                     xg_nrounds,
#                                     xg_subsample,
#                                     xg_objective,
#                                     xg_min_child_weight,
#                                     sep = "_")) 
# 
# acc_all_best_param_df <- acc_all_best_param_df %>%
#   dplyr::mutate(model_param = paste(continent_adj,
#                                     estimation_type,
#                                     target_var,
#                                     feature_type,
#                                     xg_max.depth,
#                                     xg_eta,
#                                     xg_nthread,
#                                     xg_nrounds,
#                                     xg_subsample,
#                                     xg_objective,
#                                     xg_min_child_weight,
#                                     sep = "_")) 

# Dataset using best parameters ------------------------------------------------
# Test multiple parameters; here, only keep best set of parameters for each model
# and country
acc_all_best_param_df <- acc_all_df %>%
  group_by(country, 
           n,
           level_change,
           estimation_type, estimation_type_clean,
           target_var, target_var_clean, 
           feature_type, feature_type_clean) %>%
  slice_max(order_by = r2, n = 1) 

# Export data ------------------------------------------------------------------
#### All data
saveRDS(acc_all_df, 
        file.path(data_dir, SURVEY_NAME, "FinalData", "pov_estimation_results",
                  "accuracy_appended.Rds"))

#### Best params
saveRDS(acc_all_best_param_df, 
        file.path(data_dir, SURVEY_NAME, "FinalData", "pov_estimation_results",
                  "accuracy_appended_bestparam.Rds"))

#### Best parameter set, within country cv - levels
# xg_param_set_use <- acc_all_df %>%
#   dplyr::filter(estimation_type %in% "within_country_cv",
#                 feature_type %in% "all",
#                 level_change %in% "levels") %>%
#   group_by(xg_param_set) %>%
#   summarise(r2 = mean(r2, na.rm = T)) %>%
#   arrange(-r2) %>%
#   head(1) %>%
#   pull(xg_param_set) 
# 
# acc_all_df_country_levels <- acc_all_df %>%
#   dplyr::filter(xg_param_set %in% xg_param_set_use,
#                 level_change %in% "levels")
# 
# saveRDS(acc_all_df_country_levels, 
#         file.path(data_dir, SURVEY_NAME, "FinalData", "pov_estimation_results",
#                   "accuracy_appended_bestparam_within_country_cv_levels.Rds"))
# 
# #### Best parameter set, within country cv - changes
# xg_param_set_use <- acc_all_df %>%
#   dplyr::filter(estimation_type %in% "within_country_cv",
#                 feature_type %in% "all_changes",
#                 level_change %in% "changes") %>%
#   group_by(xg_param_set) %>%
#   summarise(r2 = mean(r2, na.rm = T)) %>%
#   arrange(-r2) %>%
#   head(1) %>%
#   pull(xg_param_set) 
# 
# acc_all_df_country_changes <- acc_all_df %>%
#   dplyr::filter(xg_param_set %in% xg_param_set_use,
#                 level_change %in% "changes") 
# 
# saveRDS(acc_all_df_country_changes, 
#         file.path(data_dir, SURVEY_NAME, "FinalData", "pov_estimation_results",
#                   "accuracy_appended_bestparam_within_country_cv_changes.Rds"))
# 
# #### Dataset for each paramet set
# # for(param_set_i in unique(acc_all_df$xg_param_set)){
# #   saveRDS(acc_all_df[acc_all_df$xg_param_set %in% param_set_i,], 
# #           file.path(data_dir, SURVEY_NAME, "FinalData", "pov_estimation_results",
# #                     paste0("accuracy_appended_paramset_",param_set_i,".Rds")))
# # }
# 

