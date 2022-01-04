# Compare Poverty Estimate with RWI

# Load Data --------------------------------------------------------------------
survey_df <- readRDS(file.path(data_dir, "DHS", "FinalData", "Merged Datasets", 
                               "survey_alldata_clean_predictions.Rds"))

# Prep data --------------------------------------------------------------------
# TODO: Decide whether to use pca_allvars or wealth_score
# Maybe use wealth_score if fb_rwi is trained on wealth score?
r2_df <- survey_df %>%
  dplyr::filter(!is.na(fb_rwi)) %>%
  group_by(country_code) %>%
  dplyr::summarise(r2_estimate = cor(pca_allvars, predict_pca_allvars_best)^2,
                   r2_fbrwi    = cor(pca_allvars, fb_rwi)^2,
                   cor_estimate_fbrwi = cor(predict_pca_allvars_best, fb_rwi),
                   N_obs = n()) %>%
  ungroup() %>%
  dplyr::filter(N_obs > 10)

r2_df <- survey_df %>%
  dplyr::filter(!is.na(fb_rwi)) %>%
  group_by(country_code) %>%
  dplyr::summarise(r2_estimate = cor(wealth_index_score, predict_wealth_index_score_best)^2,
                   r2_fbrwi    = cor(wealth_index_score, fb_rwi)^2,
                   cor_estimate_fbrwi = cor(predict_wealth_index_score_best, fb_rwi),
                   N_obs = n()) %>%
  ungroup() %>%
  dplyr::filter(N_obs > 10)

# Figures ----------------------------------------------------------------------
p_boxplot <- r2_df %>%
  pivot_longer(cols = -c(country_code,
                         cor_estimate_fbrwi,
                         N_obs)) %>%
  dplyr::mutate(name = case_when(
    name == "r2_fbrwi" ~ "Facebook RWI",
    name == "r2_estimate" ~ "Model Estimate"
  )) %>%
  ggplot() +
  geom_boxplot(aes(x = value,
                   y = name),
               fill = "lightskyblue1") +
  labs(x = expression(r^2),
       y = NULL,
       title = "A. Distribution of accuracy using model and Facebook RWI") +
  scale_x_continuous(limits = c(0,1)) +
  theme_minimal() +
  theme(axis.text.y = element_text(face = "bold", color = "black"),
        plot.title = element_text(face = "bold"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title.position = "plot")

p_hist <- r2_df %>%
  dplyr::mutate(r2_diff = r2_fbrwi - r2_estimate) %>%
  ggplot() +
  geom_vline(xintercept = 0) +
  geom_histogram(aes(x = r2_diff),
                 fill = "lightskyblue1",
                 color = "black") +
  labs(x = expression(r^2~using~Model~Estimate~-r^2~using~Facebook~RWI),
       y = "N Countries",
       title = "B. Difference in accuracy between model estimate and Facebook RWI") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold"),
        plot.title.position = "plot") 

p <- ggarrange(p_boxplot,
               p_hist,
               ncol = 1)

ggsave(p, filename = file.path(figures_global_dir, "compare_rwi.png"),
       height = 4, width = 6.25)

# Model Estimate vs FB RWI Correlation -----------------------------------------
p_cor <- r2_df %>%
  ggplot(aes(x = cor_estimate_fbrwi)) +
  geom_histogram(fill = "lightskyblue1",
                 color = "black") +
  labs(x = "Within Country Correlation",
       y = "N Countries",
       title = "Distribution of correlation between model estimate\nand Facebook RWI across countries") +
  scale_x_continuous(limits = c(0, 1)) +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold"),
        plot.title.position = "plot") 

ggsave(p_cor, filename = file.path(figures_global_dir, "rwi_estimate_cor.png"),
       height = 3, width = 5)

# survey_df %>%
#   ggplot(aes(x = predict_pca_allvars_best,
#              y = fb_rwi)) +
#   geom_point(color = "dodgerblue4",
#              alpha = 0.5) +
#   labs(x = "Model Estimate",
#        y = "Facebook RWI",
#        title = "Model Estimate vs Facebook RWI") +
#   theme_minimal() +
#   theme(plot.title = element_text(face = "bold"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank()) +
#   facet_wrap(~country_name)

# Check countries --------------------------------------------------------------
r2_df %>%
  dplyr::mutate(r2_diff = r2_estimate - r2_fbrwi) %>%
  arrange(r2_diff) %>%
  head()



