# ML Changes
# Explain Variation

# Load data --------------------------------------------------------------------
accu_df <- readRDS(file.path(data_dir, SURVEY_NAME, "FinalData", "pov_estimation_results",
                        "accuracy_appended_bestparam.Rds"))

accu_df <- accu_df %>%
  dplyr::filter(level_change %in% "changes")

cluster_df <- readRDS(file.path(data_dir, SURVEY_NAME, "FinalData", "Merged Datasets",
                                "survey_alldata_clean_changes_cluster_predictions.Rds"))

district_df <- readRDS(file.path(data_dir, SURVEY_NAME, "FinalData", "Merged Datasets",
                                 "survey_alldata_clean_changes_cluster_predictions_district.Rds"))

# Explain variation: scatter plots ---------------------------------------------
line_color <- "darkorange"
accu_sum_df <- accu_df %>%
  ungroup() %>%
  dplyr::filter(feature_type %in% "all_changes",
                estimation_type %in% "best", # best "within_country_cv",
                target_var %in% "pca_allvars")

p_scatter <- accu_sum_df %>%
  dplyr::mutate(wdi_population = log(wdi_population),
                wdi_gdp_pc     = log(wdi_gdp_pc)) %>%
  dplyr::select(r2, 
                year_diff,
                pca_allvars_sd_change,
                ntlharmon_avg_sd_change,
                wdi_gdp_pc,
                pca_allvars_avg_change,
                ntlharmon_avg_change) %>%
  pivot_longer(cols = -r2) %>%
  dplyr::mutate(name = case_when(
    name == "pca_allvars_avg_change" ~ "A. Average Change in\nWealth Index",
    name == "ntlharmon_avg_change" ~ "B. Average change in\nNighttime Lights",
    name == "pca_allvars_sd_change" ~ "C. Std. Dev. of\nWealth Index Change",
    name == "ntlharmon_avg_sd_change" ~ "D. Std. Dev. of\nNTL Change",
    name == "year_diff" ~ "E. Year Difference\nBetween Surveys",
    name == "wdi_gdp_pc" ~ "F. GDP Per Capita, logged",
    TRUE ~ name
  )) %>%
  ggplot(aes(x = value,
             y = r2)) +
  geom_smooth(method='lm',
              se = F,
              color = line_color) +
  geom_point() +
  #stat_poly_eq(small.r = T) +
  stat_cor(aes(label = paste(gsub("R", "r", ..rr.label..), ..p.label.., sep = "*`,`~")),
           label.x.npc = "left",
           color = "firebrick3") +
  labs(x = NULL,
       y = "r2:\nTrue vs.\nPredicted\nAsset Index" ) +
  theme_classic() +
  theme(axis.title.y = element_text(angle = 0,
                                    vjust = 0.5),
        strip.background = element_blank(),
        strip.text = element_text(face = "bold",
                                  color = "black",
                                  hjust = 0,
                                  size = 12)) +
  facet_wrap(~name,
             scales = "free_x")

# Boxplot by income and unit ---------------------------------------------------
cluster_sum_df <- cluster_df %>%
  group_by(country_code, income) %>%
  dplyr::summarise(cor = cor(pca_allvars, predict_pca_allvars_best)) %>%
  ungroup() %>%
  mutate(unit = "Village") %>%
  mutate(r2 = cor^2)

# district_sum_df <- cluster_df %>%
#   
#   group_by(country_code, income, gadm_uid) %>%
#   dplyr::summarise(pca_allvars = mean(pca_allvars),
#                    predict_pca_allvars_best = mean(predict_pca_allvars_best)) %>%
#   
#   group_by(country_code, income) %>%
#   dplyr::summarise(cor = cor(pca_allvars, predict_pca_allvars_best)) %>%
#   
#   ungroup() %>%
#   mutate(unit = "District") %>%
#   mutate(r2 = cor^2)

district_sum_df <- district_df %>%
  distinct(country_code, income, r2) %>%
  dplyr::mutate(unit = "District")

cor_df <- bind_rows(cluster_sum_df,
                    district_sum_df)

p_bar <- cor_df %>%
  ggplot(aes(x = r2, 
             y = income,
             fill = unit)) +
  geom_boxplot() +
  # geom_half_boxplot(color = "black") +
  # geom_half_point(pch = 21,
  #                 color = "black") +
  labs(x = expression(r^2),
       y = NULL,
       fill = "Unit",
       title = "G. Model performance by income group and unit") +
  scale_fill_manual(values = c("lightblue1",
                                "tan1"),
                     guide = guide_legend(reverse = T)) +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = c(0.85, 0.85),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        plot.title = element_text(face = "bold"),
        plot.title.position = "plot",
        axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black", face = "bold", size = 12)) #+
  #coord_flip()

# Arrange/export ---------------------------------------------------------------
p <- ggarrange(p_scatter,
               p_bar, 
               nrow = 1,
               widths = c(0.6, 0.4))

ggsave(p,
       filename = file.path(figures_global_dir, "ml_changes_explain.png"),
       height = 6,
       width = 14)

# Stats ------------------------------------------------------------------------





## Average
cluster_sum_df %>%
  pull(r2) %>%
  mean()

district_sum_df %>%
  pull(r2) %>%
  mean()

## 75th, by income
district_sum_df %>%
  filter(income == "Upper middle income") %>%
  pull(r2) %>%
  summary()

district_sum_df %>%
  filter(income == "Low income") %>%
  pull(r2) %>%
  summary()






