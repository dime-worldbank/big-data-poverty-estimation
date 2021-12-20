# Feature set performance

# TODO
# (1) Some way to show pooled?
# (2) Show numbers?

# Load data --------------------------------------------------------------------
acc_df <- readRDS(file.path(data_dir, SURVEY_NAME, "FinalData", "pov_estimation_results",
                            "accuracy_appended.Rds"))

acc_df <- acc_df %>%
  dplyr::filter(target_var %in% c("pca_allvars",
                                  "pca_nonphysicalvars",
                                  "pca_physicalvars"),
                estimation_type %in% "Within Country")

# Prep Data --------------------------------------------------------------------
acc_all_df <- acc_df %>%
  group_by(estimation_type, feature_type, target_var, country) %>%
  dplyr::summarise(N = sum(N_fold),
                   cor = cor_country[1]) %>% # This repeats across folds
  dplyr::mutate(r2 = cor^2)

acc_all_df %>%
  ggplot(aes(x = r2,
             y = feature_type,
             fill = target_var)) +
  geom_boxplot()

