# Pakistan Poverty Estimation Results

SURVEY_NAME <- "DHS"

# Load Data --------------------------------------------------------------------
results_df <- file.path(data_dir, SURVEY_NAME, "FinalData", "results", "country_withincv") %>%
  list.files(full.names = T) %>%
  str_subset("withincv_results_") %>%
  map_df(read.csv) %>%
  dplyr::select(-X)

pred_df <- file.path(data_dir, SURVEY_NAME, "FinalData", "results", "country_withincv") %>%
  list.files(full.names = T) %>%
  str_subset("withincv_predicted_values_") %>%
  map_df(read.csv) %>%
  dplyr::select(-X)

# Export Data ------------------------------------------------------------------
saveRDS(results_df, file.path(data_dir, SURVEY_NAME, "FinalData", "results", "results.Rds"))
saveRDS(pred_df, file.path(data_dir, SURVEY_NAME, "FinalData", "results", "predicted_values.Rds"))

