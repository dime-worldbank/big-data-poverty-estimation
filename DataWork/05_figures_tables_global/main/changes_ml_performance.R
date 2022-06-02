# ML Performance: Predicting Changes

# TODO: Include N // by N
# TODO: By year_diff (merge in)

# Load data --------------------------------------------------------------------
df <- readRDS(file.path(data_dir, SURVEY_NAME, "FinalData", "pov_estimation_results",
                        "accuracy_appended.Rds"))

df <- df %>%
  dplyr::filter(level_change %in% "changes",
                xg_param_set %in% "10_0_1_4_50_0_3_reg_squarederror")

df %>%
  dplyr::filter(feature_type %in% "all_changes") %>%
  ggplot() +
  geom_boxplot(aes(x = r2,
                   y = estimation_type_clean))

df %>%
  dplyr::filter(estimation_type %in% "best") %>%
  ggplot() +
  geom_boxplot(aes(x = r2,
                   y = feature_type_clean))
