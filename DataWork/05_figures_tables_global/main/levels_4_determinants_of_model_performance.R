# Determinants of Model Performance

# TO PREDICT ON
# 1. Can also predict on continent (simple model)
# 2. Survey date
# 3. Inequality

set.seed(42)

R2_SCATTER_TEXT_SIZE <- 3

# Load data --------------------------------------------------------------------
#results_df <- readRDS(file.path(data_dir, SURVEY_NAME, "FinalData", "pov_estimation_results",
#                                "accuracy_appended_bestparam_within_country_cv_levels.Rds"))
results_df <- readRDS(file.path(data_dir, SURVEY_NAME, "FinalData", "pov_estimation_results",
                                "accuracy_appended_bestparam.Rds"))

results_df <- results_df %>%
  dplyr::filter(level_change == "levels") %>%
  mutate(feature_type_clean = case_when(
    feature_type_clean == "Daytime Imagery: Avg. & Std. Dev." ~ "Daytime Imagery:\nAvg. & Std. Dev.",
    TRUE ~ feature_type_clean
  ))

# wdi_df <- readRDS(file.path(data_dir, "WDI", "FinalData", "wdi.Rds"))
# 
# fb_wide_df <- readRDS(file.path(fb_marketing_dir,  "FinalData", "country_level_mau", 
#                                 "Individual Datasets",
#                                 "country_level_mau.Rds"))
# 
# survey_df <- readRDS(file.path(data_dir, SURVEY_NAME, "FinalData", "Merged Datasets", 
#                                "survey_alldata_clean.Rds"))
# 
# # Merge data -------------------------------------------------------------------
# #### Prep data for merging
# wdi_df <- wdi_df %>%
#   dplyr::select(-c(iso3c, country, year, capital, longitude, latitude))
# 
# fb_wide_df <- fb_wide_df %>%
#   dplyr::rename(iso2 = country_iso2)
# 
# survey_sum_df <- survey_df %>%
#   group_by(iso2) %>%
#   dplyr::summarise(pca_allvars_sd = sd(pca_allvars),
#                    pca_allvars_mean = mean(pca_allvars),
#                    prop_urban = mean(urban_rural %in% "U"),
#                    survey_year = year[1],
#                    N_dhs_obs = n()) %>%
#   ungroup()
# 
# #### Merge
# results_df <- results_df %>%
#   left_join(wdi_df, by = "iso2") %>%
#   left_join(fb_wide_df, by = "iso2") %>%
#   left_join(survey_sum_df, by = "iso2")
# 
# # Construct variables ----------------------------------------------------------
# results_df <- results_df %>%
#   ungroup() %>%
#   dplyr::mutate(prop_pop_on_fb = estimate_mau_1 / wdi_population,
#                 income = income %>% as.character() %>% as.factor() %>%
#                   relevel(ref = "Low income"))

# Analysis ---------------------------------------------------------------------
#### Dataset subsets for analysis
results_fb_sum_df <- results_df %>%
  dplyr::filter(feature_type %in% "fb",
                estimation_type %in% "best",
                target_var %in% "pca_allvars_mr")

results_all_sum_df <- results_df %>%
  dplyr::filter(feature_type %in% "all",
                estimation_type %in% "best",
                target_var %in% "pca_allvars_mr")

# Scatterplots -----------------------------------------------------------------
p_all_gdp_scatter <- results_all_sum_df %>%
  dplyr::filter(!is.na(wdi_gdp_pc)) %>%
  dplyr::mutate(wdi_gdp_pc_ln = log(wdi_gdp_pc)) %>%
  ggplot(aes(y = r2,
             x = wdi_gdp_pc_ln)) +
  geom_smooth(method = lm, se = F, color = "darkorange") +
  geom_point() +
  stat_poly_eq(small.r = T) +
  #geom_richtext(aes(label = paste0("Cor = ", round(cor(r2, wdi_gdp_pc_ln),2)),
  #                  x = 6,
  #                  y = 0.35),
  #              size = R2_SCATTER_TEXT_SIZE) +
  theme_classic() +
  labs(x = "GDP Per Capita, Logged",
       title = expression(bold(A.~Model~r^2~vs.~GDP~Per~Capita)),
       subtitle = "Performance using all features",
       y = expression(Model~r^2))

p_all_wealthsd_scatter <- results_all_sum_df %>%
  ggplot(aes(y = r2,
             x = pca_allvars_mr_sd)) +
  geom_smooth(method = lm, se = F, color = "darkorange") +
  geom_point() +
  stat_poly_eq(small.r = T) +
  # geom_richtext(aes(label = paste0("Cor = ", round(cor(r2, prop_pop_on_fb),2)),
  #                   x = 0.4,
  #                   y = 0.6),
  #               size = R2_SCATTER_TEXT_SIZE) +
  theme_classic() +
  labs(x = "Wealth Index, Standard Deviation",
       title = expression(bold(C.~Model~r^2~vs.~Wealth~Index~Standard~Deviation)),
       subtitle = "Performance using all features",
       y = expression(Model~r^2))

p_all_viirs_scatter <- results_all_sum_df %>%
  ggplot(aes(y = r2,
             x = viirs_avg_rad_sd)) +
  geom_smooth(method = lm, se = F, color = "darkorange") +
  geom_point() +
  stat_poly_eq(small.r = T) +
  # geom_richtext(aes(label = paste0("Cor = ", round(cor(r2, prop_pop_on_fb),2)),
  #                   x = 0.4,
  #                   y = 0.6),
  #               size = R2_SCATTER_TEXT_SIZE) +
  theme_classic() +
  labs(x = "Nighttime Lights, Standard Deviation",
       title = expression(bold(C.~Model~r^2~vs.~Nighttime~Lights~Standard~Deviation)),
       subtitle = "Performance using all features",
       y = expression(Model~r^2))

p_fb_propfb_scatter <- results_fb_sum_df %>%
  dplyr::filter(!is.na(prop_pop_on_fb)) %>%
  ggplot(aes(y = r2,
             x = prop_pop_on_fb)) +
  geom_smooth(method = lm, se = F, color = "darkorange") +
  geom_point() +
  stat_poly_eq(small.r = T) +
  # geom_richtext(aes(label = paste0("Cor = ", round(cor(r2, prop_pop_on_fb),2)),
  #                   x = 0.4,
  #                   y = 0.6),
  #               size = R2_SCATTER_TEXT_SIZE) +
  theme_classic() +
  labs(x = "Proportion of Population on Facebook",
       title = expression(bold(D.~Model~r^2~vs.~Prop.~of~Pop.~on~Facebook)),
       subtitle = "Performance using only Facebook features",
       y = expression(Model~r^2))

# Boxplots across all feature types --------------------------------------------
results_best_df <- results_df %>%
  dplyr::filter(estimation_type %in% "best",
                target_var %in% "pca_allvars_mr") %>%
  dplyr::mutate(feature_type_clean = feature_type_clean %>% 
                  as.character()) 

## Arrange factors alphabetically
results_best_df$feature_type_clean <- results_best_df$feature_type_clean %>%
  factor(levels = results_best_df$feature_type_clean %>% 
           unique() %>% 
           sort())

## Income GDP Boxplot
p_boxplot_income <- results_best_df %>%
  dplyr::mutate(income = case_when(
    income %in% "Low income" ~ "Low\nincome",
    income %in% "Lower middle income" ~ "Lower middle\nincome",
    income %in% "Upper middle income" ~ "Upper middle\nincome"
  )) %>%
  ggplot(aes(x = r2,
             y = feature_type_clean %>% fct_rev(),
             fill = income)) +
  geom_boxplot() +
  theme_classic() +
  scale_fill_brewer(palette = "Accent",
                    guide = guide_legend(reverse = FALSE)) +
  labs(x = expression(r^2),
       y = NULL,
       title = "E. Model performance by income group across all feature types",
       fill = "Income Group")

# Arrange ----------------------------------------------------------------------
theme_scatter <- theme(plot.title = element_text(face = "bold", size = 9),
                       plot.subtitle = element_text(size = 8),
                       axis.title.x = element_text(size = 8))

p_l <- ggarrange(p_all_gdp_scatter + 
                   labs(plot.title = "A. Model Performance vs. GDP Per Capita") + 
                   theme_scatter,
                 p_all_wealthsd_scatter + labs(plot.title = "A.") + theme_scatter,
                 p_all_viirs_scatter + labs(plot.title = "A.") + theme_scatter,
                 p_fb_propfb_scatter + labs(plot.title = "A.") + theme_scatter,
                 ncol = 1)

p_all <- ggarrange(p_l,
                   p_boxplot_income + 
                     theme(legend.position = "bottom",
                           axis.text.y = element_text(color = "black"),
                           legend.margin=margin(l = -2, unit='cm')) +
                     theme_scatter +
                     guides(fill=guide_legend(nrow=1, byrow=T)),
                   widths = c(0.4, 0.6),
                   nrow = 1)

ggsave(p_all,
       filename = file.path(figures_global_dir, "explain_var_in_r2.png"),
       height = 8,
       width = 9.1)

