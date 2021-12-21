# Country results

pad0 <- function(x) ifelse(x <= 9, paste0("0", x), paste0(x))

# Load data --------------------------------------------------------------------
results_df <- readRDS(file.path(data_dir, SURVEY_NAME, "FinalData", "pov_estimation_results",
                                "accuracy_appended.Rds"))

# Prep data --------------------------------------------------------------------
results_sub_df <- results_df %>%
  dplyr::filter(estimation_type %in% "best",
                target_var %in% "pca_allvars") %>%
  dplyr::filter(!(feature_type %in% c("all")))

results_sub_df <- results_sub_df %>%
  arrange(-r2) %>%
  group_by(country_name) %>%
  dplyr::mutate(feature_rank = paste0("rank_", pad0(1:n()))) %>%
  ungroup() %>%
  dplyr::mutate(feature_rank = feature_rank %>% fct_rev()) %>%
  dplyr::mutate(country_name = country_name %>% fct_rev())

# Figure -----------------------------------------------------------------------
p <- results_sub_df %>%
  ggplot(aes(x = feature_type_clean,
             y = country_name,
             fill = r2,
             label = round(r2, 2))) +
  geom_tile(color = "white") +
  geom_text(color = "black",
            size = 2.5) +
  scale_fill_distiller(palette = "GnBu",
                       direction = 0) +
  labs(y = NULL,
       x = "Feature Set",
       fill = expression(r^2)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.2))

ggsave(p, filename = file.path(figures_global_dir, "country_featureset_r2.png"),
       height = 9,
       width = 5)



