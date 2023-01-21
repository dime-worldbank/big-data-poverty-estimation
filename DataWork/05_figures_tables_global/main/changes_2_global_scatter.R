# Global Scatterplots

# Load data --------------------------------------------------------------------
cluster_df <- readRDS(file.path(data_dir, SURVEY_NAME, "FinalData", "Merged Datasets",
                                "survey_alldata_clean_changes_cluster_predictions.Rds"))

district_df <- readRDS(file.path(data_dir, SURVEY_NAME, "FinalData", "Merged Datasets",
                                 "survey_alldata_clean_changes_cluster_predictions_district.Rds"))

# Prep data --------------------------------------------------------------------
cluster_all_df <- cluster_df %>%
  dplyr::mutate(continent_adj = "All") #%>%
  # group_by(continent_adj) %>%
  # mutate(r2 = cor(pca_allvars, predict_pca_allvars_best)^2) %>%
  # ungroup() %>%
  # mutate(continent_adj = paste0(continent_adj, ": r2 = ", round(r2, 2)))

district_all_df <- district_df %>%
  dplyr::mutate(continent_adj = "All") #%>%
  # group_by(continent_adj) %>%
  # mutate(r2 = cor(pca_allvars, predict_pca_allvars_best)^2) %>%
  # ungroup() %>%
  # mutate(continent_adj = paste0(continent_adj, ": r2 = ", round(r2, 2)))

cluster_clean_df <- bind_rows(cluster_df,
                              cluster_all_df) %>%
  mutate(continent_adj = continent_adj %>%
           factor(levels = c("All", "Africa", "Americas", "Eurasia")))

district_clean_df <- bind_rows(district_df,
                               district_all_df) %>%
  mutate(continent_adj = continent_adj %>%
           factor(levels = c("All", "Africa", "Americas", "Eurasia")))

# Figures ----------------------------------------------------------------------
p1 <- cluster_clean_df %>%
  ggplot(aes(x = pca_allvars,
             y = predict_pca_allvars_best)) +
  geom_point(size = 0.1) +
  stat_poly_eq(color = "red", small.r=T) +
  labs(x = "True Change in Wealth Index",
       y = "Estimated Change in\nWealth Index",
       title = "A. Estimated vs. true change in wealth index [cluster]",
       color = NULL) +
  theme_classic() +
  theme(strip.background = element_blank(),
        strip.text = element_text(face = "bold",size = 11),
        plot.title = element_text(face = "bold")) +
  facet_wrap(~continent_adj,
             nrow = 1,
             scales = "free") 

p2 <- district_clean_df %>%
  ggplot(aes(x = truth,
             y = prediction)) +
  geom_point(size = 0.1) +
  stat_poly_eq(color = "red", small.r=T) +
  labs(x = "True Change in Wealth Index",
       y = "Estimated Change in\nWealth Index",
       title = "B. Estimated vs. true change in wealth index [district]",
       color = NULL) +
  theme_classic() +
  theme(strip.background = element_blank(),
        strip.text = element_text(face = "bold",size = 11),
        plot.title = element_text(face = "bold")) +
  facet_wrap(~continent_adj,
             nrow = 1,
             scales = "free")

# Arrange / Export -------------------------------------------------------------
p <- ggarrange(p1, p2, ncol = 1)

ggsave(p,
       filename = file.path(figures_global_dir, "changes_scatter_global_cont.png"),
       height = 5,
       width = 8)
