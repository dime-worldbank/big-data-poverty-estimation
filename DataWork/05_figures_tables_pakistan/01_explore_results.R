# Main Results

# Load data --------------------------------------------------------------------
results_df <- readRDS(file.path(data_dir, SURVEY_NAME, "FinalData", "pov_estimation_results",
                                "accuracy_appended.Rds"))

results_df <- results_df %>%
  dplyr::filter(country_name %in% "Pakistan")

# Best Estimation Approach: Table ----------------------------------------------
a <- results_df %>%
  dplyr::filter(feature_type %in% "all")





