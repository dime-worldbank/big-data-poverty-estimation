# Compare Facebook RWI Values

# Load data --------------------------------------------------------------------
pakpoints_df <- readRDS(file.path(data_dir, "PAK_POINTS", 
                                  "FinalData", "Merged Datasets", "survey_alldata_clean_predictions.Rds"))

# Figure -----------------------------------------------------------------------
cor_value <- cor(pakpoints_df$predict_wealth_index_score,
                 pakpoints_df$rwi)

p <- pakpoints_df %>%
  ggplot() +
  geom_point(aes(x = predict_wealth_index_score,
                 y = rwi),
             alpha = 0.4,
             size = 0.2) +
  geom_label(aes(x = -100000,
                y = 1.2),
            label = paste0("Cor: ", round(cor_value, 2))) +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold")) +
  labs(x = "Predicted Wealth Score",
       y = "FB RWI",
       title = "Predicted Wealth Score vs\nFacebook Relative Wealth Index")

ggsave(p, 
       filename = file.path(figures_pak_dir, "fbrw_predwealthscore_scatter.png"),
       height = 4.75,
       width = 5)







