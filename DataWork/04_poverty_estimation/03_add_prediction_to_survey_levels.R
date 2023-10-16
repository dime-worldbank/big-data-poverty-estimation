# Add select predictions to survey

# Load Data --------------------------------------------------------------------
survey_df <- readRDS(file.path(data_dir, SURVEY_NAME, "FinalData", "Merged Datasets", 
                               "survey_alldata_clean.Rds"))

pred_df <- readRDS(file.path(data_dir, SURVEY_NAME, "FinalData", "pov_estimation_results",
                             "predictions_appended.Rds"))

# Cleanup prediction data ------------------------------------------------------
pred_wide_df <- pred_df %>%
  dplyr::filter(level_change %in% "levels") %>%
  pivot_wider(id_cols = uid,
              names_from = c(target_var_dep, estimation_type, feature_type),
              values_from = prediction) %>%
  rename_at(vars(-uid), ~ paste0("predict_", .))

survey_df <- survey_df %>%
  left_join(pred_wide_df, by = "uid")

# Export -----------------------------------------------------------------------
saveRDS(survey_df,
        file.path(data_dir, SURVEY_NAME, "FinalData", "Merged Datasets", "survey_alldata_clean_predictions.Rds"))

