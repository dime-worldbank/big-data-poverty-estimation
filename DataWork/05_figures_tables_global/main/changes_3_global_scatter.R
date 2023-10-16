# Global Scatterplots

# Load data --------------------------------------------------------------------
cluster_df <- readRDS(file.path(data_dir, SURVEY_NAME, "FinalData", "Merged Datasets",
                                "survey_alldata_clean_changes_cluster_predictions.Rds"))

district_df <- readRDS(file.path(data_dir, SURVEY_NAME, "FinalData", "Merged Datasets",
                                 "predictions_changes_district_appended.Rds"))
district_df <- district_df %>%
  dplyr::filter(estimation_type %in% "best")

# Prep data --------------------------------------------------------------------
cluster_all_df <- cluster_df %>%
  dplyr::mutate(continent_adj = "All") 

district_all_df <- district_df %>%
  dplyr::mutate(continent_adj = "All") 

cluster_clean_df <- bind_rows(cluster_df,
                              cluster_all_df) %>%
  mutate(continent_adj = continent_adj %>%
           factor(levels = c("All", "Africa", "Americas", "Eurasia")))

district_clean_df <- bind_rows(district_df,
                               district_all_df) %>%
  mutate(continent_adj = continent_adj %>%
           factor(levels = c("All", "Africa", "Americas", "Eurasia")))

#### Remove outliers
out_lim <- as.numeric(quantile(district_clean_df$prediction, 0.9999))

district_clean_df <- district_clean_df %>%
  dplyr::filter(prediction < out_lim)

# Figures ----------------------------------------------------------------------
#### Cluster
v <- c(cluster_clean_df$predict_pca_allvars_best_all_changes, cluster_clean_df$pca_allvars)

r2_R2_df <- cluster_clean_df %>%
  group_by(continent_adj) %>%
  dplyr::summarise(r2 = cor(predict_pca_allvars_best_all_changes, pca_allvars)^2,
                   R2 = R2(predict_pca_allvars_best_all_changes, pca_allvars, form = "traditional")) %>%
  ungroup() %>%
  mutate(label = paste0("r<sup>2</sup>: ", round(r2, 2), "<br>R<sup>2</sup>: ", round(R2, 2)))

p1 <- cluster_clean_df %>%
  ggplot(aes(x = predict_pca_allvars_best_all_changes,
             y = pca_allvars)) +
  geom_point(size = 0.1) +
  #stat_poly_eq(color = "red", small.r=T) +
  geom_richtext(data = r2_R2_df,
                aes(x = -2.8, y = 4.2, label = label),
                fill = NA, 
                label.color = NA,
                color = "red") +
  labs(x = "Estimated Change in Wealth Index",
       y = "True Change in\nWealth Index",
       title = "A. Estimated vs. true change in wealth index [cluster]",
       color = NULL) +
  theme_classic() +
  theme(strip.background = element_blank(),
        strip.text = element_text(face = "bold",size = 11),
        plot.title = element_text(face = "bold")) +
  facet_wrap(~continent_adj,
             nrow = 1,
             scales = "fixed") +
  scale_x_continuous(limits = c(min(v), max(v))) +
  scale_y_continuous(limits = c(min(v), max(v)))

#### District
v <- c(district_clean_df$truth, district_clean_df$prediction)

r2_R2_df <- district_clean_df %>%
  group_by(continent_adj) %>%
  dplyr::summarise(r2 = cor(prediction, truth)^2,
                   R2 = R2(prediction, truth, form = "traditional")) %>%
  ungroup() %>%
  mutate(label = paste0("r<sup>2</sup>: ", round(r2, 2), "<br>R<sup>2</sup>: ", round(R2, 2)))

p2 <- district_clean_df %>%
  ggplot(aes(x = truth,
             y = prediction)) +
  geom_point(size = 0.1) +
  #stat_poly_eq(color = "red", small.r=T) +
  geom_richtext(data = r2_R2_df,
                aes(x = -2, y = 4.2, label = label),
                fill = NA, 
                label.color = NA,
                color = "red") +
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
             scales = "fixed") +
  scale_x_continuous(limits = c(min(v), max(v))) +
  scale_y_continuous(limits = c(min(v), max(v))) 

# Arrange / Export -------------------------------------------------------------
p <- ggarrange(p1, p2, ncol = 1)

ggsave(p,
       filename = file.path(figures_global_dir, "changes_scatter_global_cont.png"),
       height = 5,
       width = 8)
