# r2 vs R2

# Load data --------------------------------------------------------------------
df <- readRDS(file.path(data_dir, SURVEY_NAME, "FinalData", "pov_estimation_results",
                        "accuracy_appended.Rds"))

#### Best params
df <- readRDS(file.path(data_dir, SURVEY_NAME, "FinalData", "pov_estimation_results",
                        "accuracy_appended_bestparam.Rds"))


df %>%
  dplyr::filter(feature_type %in% "all",
                ml_model_type %in% "xgboost",
                level_change %in% "levels",
                estimation_type %in% "best") %>%
  ggplot() +
  geom_boxplot(aes(x = coef_det))
