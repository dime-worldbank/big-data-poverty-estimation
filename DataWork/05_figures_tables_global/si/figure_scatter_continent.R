# Scatterplot of pooled results by continent

# Load/prep data ---------------------------------------------------------------
survey_df <- readRDS(file.path(data_dir, "DHS", "FinalData", "Merged Datasets",
                               "survey_alldata_clean_predictions.Rds"))

survey_df <- survey_df %>%
  filter(most_recent_survey %in% T) %>%
  
  group_by(continent_adj) %>%
  dplyr::mutate(r2_a = cor(pca_allvars_mr, 
                           predict_pca_allvars_mr_global_country_pred_all)^2,
                r2_u = cor(pca_allvars_mr[urban_rural == "U"], 
                         predict_pca_allvars_mr_global_country_pred_all[urban_rural == "U"])^2,
                r2_r = cor(pca_allvars_mr[urban_rural == "R"], 
                           predict_pca_allvars_mr_global_country_pred_all[urban_rural == "R"])^2) %>%
  ungroup() %>%
  mutate(continent_adj_r2 = paste0(continent_adj, "\n", 
                                   "r2 = ", round(r2_a, 2), "\n",
                                "r2 (Urban) = ", round(r2_u, 2), "\n",
                                "r2 (Rural) = ", round(r2_r, 2)))

district_df <- survey_df %>%
  group_by(gadm_uid, country_code, continent_adj) %>%
  dplyr::summarise(pca_allvars_mr = mean(pca_allvars_mr),
                   predict_pca_allvars_mr_global_country_pred_all = mean(predict_pca_allvars_mr_global_country_pred_all)) %>%
  ungroup() %>%
  
  group_by(continent_adj) %>%
  dplyr::mutate(r2 = cor(pca_allvars_mr, predict_pca_allvars_mr_global_country_pred_all)^2) %>%
  mutate(continent_adj_r2 = paste0(continent_adj, "\n", "r2 = ", round(r2, 2)))

# Figures ----------------------------------------------------------------------
p_c <- survey_df %>%
  mutate(urban_rural = case_when(
    urban_rural == "U" ~ "Urban",
    urban_rural == "R" ~ "Rural"
  )) %>%
  ggplot(aes(x = predict_pca_allvars_mr_global_country_pred_all,
             y = pca_allvars_mr,
             color = urban_rural)) +
  geom_point(size = 0.25,
             alpha = 0.9) +
  scale_color_manual(values = c("chartreuse4", "chocolate1")) +
  facet_wrap(~continent_adj_r2) +
  labs(color = NULL,
       title = "A. Estimated vs. True Wealth Scores, Cluster Level",
       y = "True Wealth Asset Index",
       x = "Estimated Wealth Asset Index") +
  theme_minimal() +
  theme(legend.position = "top",
        legend.box.background = element_rect(colour = "black"),
        plot.title = element_text(face = "bold", size = 16),
        strip.text = element_text(face = "bold")) 

p_d <- district_df %>%
  ggplot(aes(x = predict_pca_allvars_mr_global_country_pred_all,
             y = pca_allvars_mr)) +
  geom_point(size = 0.25) +
  facet_wrap(~continent_adj_r2) +
  labs(color = NULL,
       title = "B. Estimated vs. True Wealth Scores, District Level",
       y = "True Wealth Asset Index",
       x = "Estimated Wealth Asset Index") +
  theme_minimal() +
  theme(legend.position = "top",
        legend.box.background = element_rect(colour = "black"),
        plot.title = element_text(face = "bold", size = 16),
        strip.text = element_text(face = "bold")) 

p <- ggarrange(p_c,
               p_d,
               ncol = 1)

ggsave(p, 
       filename = file.path(figures_global_dir, "r2_scatter_pooled_by_continent.png"),
       height = 10, 
       width = 12)
 


