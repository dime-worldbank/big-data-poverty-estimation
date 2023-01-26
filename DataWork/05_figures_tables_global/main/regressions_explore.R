# Regressions: Explore

# Load data --------------------------------------------------------------------
results_df <- readRDS(file.path(data_dir, "DHS_OLD", "FinalData", "pov_estimation_results",
                             "accuracy_appended_bestparam.Rds"))

levels_df <- results_df %>%
  filter(level_change %in% "levels",
         target_var %in% "pca_allvars_mr",
         feature_type %in% "all",
         estimation_type %in% "best")

changes_df <- results_df %>%
  filter(level_change %in% "changes",
         target_var %in% "pca_allvars",
         feature_type %in% "all_changes",
         estimation_type %in% "best")

# Regressions ------------------------------------------------------------------
lm(r2 ~ pca_allvars_mr_sd + log(wdi_gdp_pc+1) + log(wdi_population), data = levels_df) %>%
  summary()

lm(r2 ~ pca_allvars_sd_change + ntlharmon_avg_sd_change, data = changes_df) %>%
  summary()



levels_df$pca_allvars_mr_sd





