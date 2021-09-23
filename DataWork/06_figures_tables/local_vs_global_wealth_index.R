# Compare DHS Wealth Index vs Computed Global Wealth Index

# Load Data --------------------------------------------------------------------
survey_df <- readRDS(file.path(dhs_dir, "FinalData", "Individual Datasets", "survey_socioeconomic.Rds"))

survey_df <- survey_df %>%
  group_by(country_code) %>%
  mutate(cor = cor(wealth_index, asset_pca_1)) %>%
  ungroup() %>%
  mutate(country_cor = paste0(country_code, "; Cor = ", round(cor, 2)))

cor_df <- survey_df %>%
  distinct(country_code, cor)

# [Stats] ----------------------------------------------------------------------
mean(cor_df$cor > 0.8)
table(cor_df$cor > 0.8)

median(cor_df$cor)
mean(cor_df$cor)

# [Figure] Correlation Histogram -----------------------------------------------
p <- cor_df %>%
  mutate(cor = as.factor(round(cor * 10)/10)) %>%
  group_by(cor) %>%
  dplyr::summarise(N = n()) %>%
  ggplot(aes(x = cor,
             y = N)) +
  geom_col(fill = "dodgerblue",
           color = "black") +
  geom_text(aes(y = N + 1,
                label = N)) +
  labs(x = "Correlation",
       y = "N Countries",
       title = "Distribution of Correlation between DHS Wealth Index
       and Global Asset Index Within Countries") +
  theme_minimal()

ggsave(p, 
       filename = file.path(figures_dir, "local_vs_global_asset_hist.png"),
       height = 4,
       width = 5)

# [Figure] Scatterplot ---------------------------------------------------------
p <- survey_df %>% 
  ggplot() +
  geom_point(aes(x = wealth_index,
                 y = asset_pca_1),
             size = 0.2) +
  labs(x = "DHS Wealth Index",
       y = "Global Asset Index") +
  theme_minimal() +
  theme(axis.title = element_text(size = 16),
        strip.text = element_text(size = 14)) +
  facet_wrap(~country_cor,
             scales = "free_y",
             ncol = 6) 

ggsave(p, 
       filename = file.path(figures_dir, "local_vs_global_asset_scatter.png"),
       height = 20,
       width = 15)
