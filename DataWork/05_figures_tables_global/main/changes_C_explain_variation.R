# ML Changes
# Explain Variation

# Load data --------------------------------------------------------------------
df <- readRDS(file.path(data_dir, SURVEY_NAME, "FinalData", "pov_estimation_results",
                        "accuracy_appended_bestparam.Rds"))

df <- df %>%
  dplyr::filter(level_change %in% "changes")

# Explain variation ------------------------------------------------------------
line_color <- "darkorange"
df_sum <- df %>%
  ungroup() %>%
  dplyr::filter(feature_type %in% "all_changes",
                estimation_type %in% "within_country_cv", # "within_country_cv",
                target_var %in% "pca_allvars")

p <- df_sum %>%
  dplyr::mutate(wdi_population = log(wdi_population)) %>%
  dplyr::select(r2, 
                year_diff,
                pca_allvars_sd_change,
                ntlharmon_avg_sd_change,
                wdi_population,
                wdi_gdp_pc) %>%
  pivot_longer(cols = -r2) %>%
  dplyr::mutate(name = case_when(
    name == "pca_allvars_sd_change" ~ "A. Std. Dev. of Asset Index Change",
    name == "ntlharmon_avg_sd_change" ~ "B. Std. Dev. of NTL Change",
    name == "year_diff" ~ "C. Year Difference",
    name == "wdi_population" ~ "D. Population, logged",
    name == "wdi_gdp_pc" ~ "E. GDP Per Capita",
    TRUE ~ name
  )) %>%
  ggplot(aes(x = value,
             y = r2)) +
  geom_smooth(method='lm',
              se = F,
              color = line_color) +
  geom_point() +
  stat_poly_eq() +
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

ggsave(p,
       filename = file.path(figures_global_dir, "ml_changes_explain.png"),
       height = 6,
       width = 14)




