# Append Poverty Estimation Results

# Load/append data -------------------------------------------------------------
acc_df <- file.path(data_dir, SURVEY_NAME, "FinalData", "pov_estimation_results", "accuracy") %>%
  list.files(pattern = "*.Rds",
             full.names = T) %>%
  map_df(readRDS)

# Export data ------------------------------------------------------------------
saveRDS(acc_df, 
        file.path(data_dir, SURVEY_NAME, "FinalData", "pov_estimation_results",
                  "accuracy_appended.Rds"))

