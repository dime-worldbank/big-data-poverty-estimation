# District Level Changes

# Load data --------------------------------------------------------------------
survey_df <- readRDS(file.path(data_dir, "DHS", "FinalData", "Merged Datasets", 
                               "survey_alldata_clean_changes_cluster_predictions.Rds"))

df <- readRDS(file.path(data_dir, "DHS", "FinalData", "pov_estimation_results",
                             "predictions_appended.Rds")) %>%
  dplyr::filter(level_change %in% "changes",
                estimation_type != "best")

# Add GADM and ISO -------------------------------------------------------------
gadm_df <- survey_df %>%
  dplyr::select(uid, gadm_uid, iso2, continent_adj)

df <- df %>%
  left_join(gadm_df, by = "uid")

# Aggregate --------------------------------------------------------------------
df <- df %>%
  
  group_by(gadm_uid, country_code, iso2, country_name, continent_adj, estimation_type, feature_type, target_var_dep) %>%
  dplyr::summarise(target_var = mean(target_var, na.rm=T),
                   prediction = mean(prediction, na.rm=T)) %>%
  ungroup() 

sum_df <- df %>%
  group_by(country_code, iso2, country_name, continent_adj, estimation_type, feature_type, target_var_dep) %>%
  dplyr::summarise(r2 = cor(target_var, prediction)^2,
                   R2 = R2(target_var, prediction, form = "traditional"),
                   n = n()) %>%
  ungroup()

# Add best estimation ----------------------------------------------------------
#### Determine best est type
best_sum_df <- sum_df %>%
  dplyr::filter(feature_type %in% "all_changes") %>%
  arrange(-R2) %>%
  group_by(country_code, iso2) %>%
  slice_max(R2, n = 1) %>%
  ungroup() %>%
  dplyr::rename(estimation_type_orig = estimation_type) %>%
  dplyr::mutate(estimation_type = "best",
                est_cnty = paste(estimation_type_orig, country_code))

best_df <- df %>%
  dplyr::filter(feature_type %in% "all_changes") %>%
  dplyr::mutate(est_cnty = paste(estimation_type, country_code))

best_df <- best_df[best_df$est_cnty %in% best_sum_df$est_cnty,] %>%
  dplyr::rename(estimation_type_orig = estimation_type) %>%
  dplyr::mutate(estimation_type = "best")

#### Add best estimation type to dataframes
df     <- bind_rows(df, best_df)
sum_df <- bind_rows(sum_df, best_sum_df)

# Merge WDI Indicators ---------------------------------------------------------
## Prep WDI
wdi_df <- readRDS(file.path(data_dir, "WDI", "FinalData", "wdi.Rds"))

wdi_df <- wdi_df %>%
  dplyr::select(-c(iso3c, country, year, capital, longitude, latitude))

## Merge
df <- df %>%
  left_join(wdi_df, by = "iso2") 

sum_df <- sum_df %>%
  left_join(wdi_df, by = "iso2") 

# Cleanup ----------------------------------------------------------------------
df <- df %>%
  dplyr::rename(truth = target_var)

# Export -----------------------------------------------------------------------
saveRDS(df, 
        file.path(data_dir, "DHS", "FinalData", "Merged Datasets",
                  "predictions_changes_district_appended.Rds"))

saveRDS(sum_df, 
        file.path(data_dir, "DHS", "FinalData", "pov_estimation_results",
                  "accuracy_changes_district_appended.Rds"))
