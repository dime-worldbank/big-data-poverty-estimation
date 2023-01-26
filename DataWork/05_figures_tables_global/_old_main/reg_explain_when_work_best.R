# When predictions work best?

# Load data --------------------------------------------------------------------
acc_df <- readRDS(file.path(data_dir, SURVEY_NAME, "FinalData", "pov_estimation_results",
                            "accuracy_appended.Rds"))

acc_df <- acc_df %>%
  dplyr::filter(target_var == "pca_allvars",
                estimation_type == "Within Country")


acc_all_df <- acc_df %>%
  dplyr::filter(feature_type == "all")

# Load/Prep Survey Data --------------------------------------------------------
survey_df <- readRDS(file.path(dhs_dir, "FinalData", "Merged Datasets", "survey_alldata_clean.Rds"))

survey_sum_df <- survey_df %>%
  group_by(country_code) %>%
  dplyr::summarise(pca_allvars_mean = mean(pca_allvars, na.rm=T),
                   pca_allvars_sd = sd(pca_allvars, na.rm = T))

# Merge ------------------------------------------------------------------------
acc_all_df <- acc_all_df %>%
  left_join(survey_df, by = "country_code")


lm(cor_fold ~ pca_allvars_mean + pca_allvars_sd + factor(continent_adj), data = acc_all_df) %>%
  summary()

