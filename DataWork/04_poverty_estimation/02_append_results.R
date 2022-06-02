# Append Poverty Estimation Results

# Load/append data -------------------------------------------------------------
acc_df <- file.path(data_dir, SURVEY_NAME, "FinalData", "pov_estimation_results", 
                    "accuracy") %>%
  list.files(pattern = "*.Rds",
             full.names = T) %>%
  map_df(readRDS)

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
    feature_type == "fb_prop" ~ "Facebook: Proportion",
    feature_type == "fb" ~ "Facebook",
    feature_type == "satellites" ~ "Satellites",
    feature_type == "viirs" ~ "Nighttime Lights",
    feature_type == "ntlharmon" ~ "Nighttime Lights",
    feature_type == "landcover" ~ "Landcover",
    feature_type == "weatherclimate" ~ "Weather/Climate",
    feature_type == "weather" ~ "Weather",
    feature_type == "gc" ~ "Landcover - GlobCover",
    feature_type == "l8" ~ "Daytime Imagery - Average",
    feature_type == "l7" ~ "Daytime Imagery - Average",
    feature_type == "osm" ~ "OpenStreetMap",
    feature_type == "pollution" ~ "Pollution",
    feature_type == "pollution_aod" ~ "Pollution",
    #feature_type == "" ~ "",
    TRUE ~ feature_type
  )) %>%
  dplyr::mutate(target_var_clean = case_when(
    target_var == "pca_allvars" ~ "Asset Index",
    target_var == "pca_nonphysicalvars" ~ "Asset Index: Non-Physical Assets",
    target_var == "pca_physicalvars" ~ "Asset Index: Physical Assets",
    target_var == "wealth_index_score" ~ "DHS Wealth Index",
    TRUE ~ target_var
  )) %>%
  dplyr::mutate(target_var_clean = factor(target_var_clean,
                                          levels = c("DHS Wealth Index",
                                                     "Asset Index",
                                                     "Asset Index: Physical Assets",
                                                     "Asset Index: Non-Physical Assets")))

# Merge with select survey variables -------------------------------------------
survey_df <- readRDS(file.path(data_dir, SURVEY_NAME, "FinalData", "Merged Datasets", 
                               "survey_alldata_clean.Rds"))

survey_df <- survey_df %>%
  distinct(country_code, country_name, continent_adj, iso2) %>%
  dplyr::rename(country = country_code)

acc_df <- acc_df %>%
  dplyr::left_join(survey_df, by = "country")

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
           xg_max.depth, xg_eta, xg_nthread, xg_nrounds, xg_subsample, xg_objective,
           country_predict_group, country, country_name, continent_adj, iso2) %>%
  dplyr::summarise(N = sum(N_fold),
                   cor = cor_country[1]) %>% # This repeats across folds
  dplyr::mutate(r2 = cor^2)

# Add rows for best estimation type WITHIN each set of parameters --------------
acc_all_best_df <- acc_all_df %>%
  group_by(country, 
           n,
           level_change,
           target_var, target_var_clean, 
           feature_type, feature_type_clean,
           xg_max.depth, xg_eta, xg_nthread, xg_nrounds, xg_subsample, xg_objective) %>%
  slice_max(order_by = r2, n = 1) %>%
  mutate(estimation_type_clean = "Using Best Training Sample\nType for Each Country",
         estimation_type = "best") %>%
  mutate(est_cat = "Best")

acc_all_df <- bind_rows(
  acc_all_df,
  acc_all_best_df
)

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

# Add variable for parameter set -----------------------------------------------
acc_all_df <- acc_all_df %>%
  dplyr::mutate(xg_param_set = paste(xg_max.depth,
                                     xg_eta,
                                     xg_nthread,
                                     xg_nrounds,
                                     xg_subsample,
                                     xg_objective, sep = "_") %>%
                  str_replace_all("[:punct:]", "_"))

# Export data ------------------------------------------------------------------
#### All data
saveRDS(acc_all_df, 
        file.path(data_dir, SURVEY_NAME, "FinalData", "pov_estimation_results",
                  "accuracy_appended.Rds"))

#### Best params
saveRDS(acc_all_best_param_df, 
        file.path(data_dir, SURVEY_NAME, "FinalData", "pov_estimation_results",
                  "accuracy_appended_bestparam.Rds"))

#### Dataset for each paramet set
for(param_set_i in unique(acc_all_df$xg_param_set)){
  saveRDS(acc_all_df[acc_all_df$xg_param_set %in% param_set_i,], 
          file.path(data_dir, SURVEY_NAME, "FinalData", "pov_estimation_results",
                    paste0("accuracy_appended_paramset_",param_set_i,".Rds")))
}


