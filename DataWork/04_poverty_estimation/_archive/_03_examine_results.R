# Examine results

# Load data --------------------------------------------------------------------
acc_all_df <- readRDS(file.path(data_dir, SURVEY_NAME, "FinalData", "pov_estimation_results",
                                "accuracy_appended.Rds"))

# Prep Data --------------------------------------------------------------------

acc_all_df %>% 
  dplyr::filter(feature_type != "all") %>% 
  ggplot(aes(x = r2,
             y = estimation_type,
             fill = factor(stat(quantile)))) +
  stat_density_ridges(
    geom = "density_ridges_gradient", calc_ecdf = TRUE,
    quantiles = 4, quantile_lines = TRUE
  ) +
  scale_fill_viridis_d(name = "Quartiles") +
  facet_wrap(~target_var)

acc_all_df %>% 
  dplyr::filter(feature_type == "all",
                target_var == "pca_allvars") %>% 
  ggplot(aes(x = r2,
             y = estimation_type,
             fill = factor(stat(quantile)))) +
  stat_density_ridges(
    geom = "density_ridges_gradient", calc_ecdf = TRUE,
    quantiles = 4, quantile_lines = TRUE
  ) +
  scale_fill_viridis_d(name = "Quartiles")




acc_all_df %>% 
  ggplot(aes(x = r2,
             y = feature_type)) +
  geom_density_ridges(
    jittered_points = TRUE, position = "raincloud",
    alpha = 0.7, scale = 0.9
  ) + 
  facet_wrap(~estimation_type)

