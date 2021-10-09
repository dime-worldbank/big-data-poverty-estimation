# Examine results

# Load data --------------------------------------------------------------------
acc_df <- readRDS(file.path(data_dir, SURVEY_NAME, "FinalData", "pov_estimation_results",
                            "accuracy_appended.Rds"))

# Examine results ---------------------------------------------------------------
acc_all_df <- acc_df %>%
  group_by(estimation_type, feature_type, target_var, country) %>%
  dplyr::summarise(N = sum(N_fold),
                   cor = cor_all[1]) %>% # This repeats across folds
  dplyr::mutate(r2 = cor^2)


