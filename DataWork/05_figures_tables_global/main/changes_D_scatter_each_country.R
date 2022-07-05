# Changes
# Scatterplot Each Country

# TODO: May need to fix to be consistent with other figure?

# Load data --------------------------------------------------------------------
survey_df <- readRDS(file.path(data_dir, SURVEY_NAME, "FinalData", "Merged Datasets",
                               "survey_alldata_clean_changes_cluster_predictions.Rds"))

# Figure -----------------------------------------------------------------------
p_pca <- survey_df %>%
  group_by(country_name) %>%
  dplyr::mutate(r2 = cor(pca_allvars,
                         predict_pca_allvars_within_country_cv_all_changes)^2) %>%
  ungroup() %>%
  dplyr::mutate(country_name = fct_reorder(country_name,
                                           -r2)) %>%
  ggplot(aes(x = pca_allvars,
             y = predict_pca_allvars_within_country_cv_all_changes)) +
  geom_point(size = 0.3) +
  stat_poly_eq(color = "firebrick3") +
  facet_wrap(~country_name) +
  theme_classic() +
  labs(x = "True Asset Index",
       y = "Predicted Asset Index") +
  theme(strip.background = element_blank(),
        strip.text = element_text(face = "bold"))

p_dhs <- survey_df %>%
  group_by(country_name) %>%
  dplyr::mutate(r2 = cor(wealth_index_score,
                         predict_wealth_index_score_within_country_cv_all_changes)^2) %>%
  ungroup() %>%
  dplyr::mutate(country_name = fct_reorder(country_name,
                                           -r2)) %>%
  ggplot(aes(x = wealth_index_score,
             y = predict_wealth_index_score_within_country_cv_all_changes)) +
  geom_point(size = 0.3) +
  stat_poly_eq(color = "firebrick3") +
  facet_wrap(~country_name) +
  theme_classic() +
  labs(x = "True DHS Wealth Index",
       y = "Predicted DHS Wealth Index") +
  theme(strip.background = element_blank(),
        strip.text = element_text(face = "bold"))


ggsave(p_pca,
       filename = file.path(figures_global_dir, "ml_changes_scatter_eachcountry_pca.png"),
       height = 12,
       width = 12)

ggsave(p_dhs,
       filename = file.path(figures_global_dir, "ml_changes_scatter_eachcountry_dhs.png"),
       height = 12,
       width = 12)


