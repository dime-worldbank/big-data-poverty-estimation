# Determinants of Model Performance

# TO PREDICT ON
# 1. Can also predict on continent (simple model)
# 2. Survey date
# 3. Inequality

set.seed(42)

R2_SCATTER_TEXT_SIZE <- 3

# Load data --------------------------------------------------------------------
results_df <- readRDS(file.path(data_dir, SURVEY_NAME, "FinalData", "pov_estimation_results",
                                "accuracy_appended.Rds"))
results_df <- results_df %>%
  dplyr::filter(xg_param_set %in% "10_0_1_4_50_0_3_reg_squarederror")

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
                target_var %in% "pca_allvars")

results_all_sum_df <- results_df %>%
  dplyr::filter(feature_type %in% "all",
                estimation_type %in% "best",
                target_var %in% "pca_allvars")

#### Income & Std Dev Wealth
p_wealthsd_income <- results_all_sum_df %>%
  dplyr::mutate(income = case_when(
    income %in% "Low income" ~ "Low\nincome",
    income %in% "Lower middle income" ~ "Lower middle\nincome",
    income %in% "Upper middle income" ~ "Upper middle\nincome"
  )) %>%
  ggplot(aes(y = pca_allvars_sd,
             x = income,
             fill = income)) +
  geom_half_boxplot(center = T) +
  geom_half_point() +
  coord_flip() +
  labs(y = "Standard Deviation of Wealth Score",
       x = NULL,
       fill = "Income\nGroup",
       title = "F. Standard deviation of wealth score by income group") +
  theme_classic() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.text = element_text(color = "black"),
        plot.title = element_text(face = "bold")) +
  scale_fill_brewer(palette = "Accent",
                    guide = guide_legend(reverse = FALSE)) 

#### Boxplot and jitterpoints of income & continent
p_all_income_box <- results_all_sum_df %>%
  dplyr::mutate(income_num = income %>% 
                  as.factor() %>% 
                  as.numeric()) %>%
  ggplot(aes(y = r2,
             x = income)) +
  geom_half_boxplot(center = T, errorbar.draw = F, fill = "gray90") +
  geom_jitter(aes(color = continent_adj,
                  x = income_num + 0.175),
              width = 0.17,
              size = 2) +
  labs(x = NULL,
       y = expression(r^2),
       color = "Continent") +
  coord_flip() +
  theme_classic() +
  scale_color_brewer(palette = "Set1")

#### Scatterplots
p_all_gdp_scatter <- results_all_sum_df %>%
  dplyr::mutate(wdi_gdp_pc_ln = log(wdi_gdp_pc)) %>%
  ggplot(aes(y = r2,
             x = wdi_gdp_pc_ln)) +
  geom_smooth(method = lm, se = F, color = "darkorange") +
  geom_point() +
  geom_richtext(aes(label = paste0("Cor = ", round(cor(r2, wdi_gdp_pc_ln),2)),
                    x = 6,
                    y = 0.35),
                size = R2_SCATTER_TEXT_SIZE) +
  theme_classic() +
  labs(x = "GDP Per Capita, Logged",
       title = expression(bold(A.~Model~r^2~vs.~GDP~Per~Capita)),
       subtitle = "Performance using all features",
       y = expression(r^2))

p_all_wealthsd_scatter <- results_all_sum_df %>%
  ggplot(aes(y = r2,
             x = pca_allvars_sd)) +
  geom_smooth(method = lm, se = F, color = "darkorange") +
  geom_point() +
  geom_richtext(aes(label = paste0("Cor = ", round(cor(r2, pca_allvars_sd),2)),
                    x = 0.6,
                    y = 0.8),
                size = R2_SCATTER_TEXT_SIZE) +
  theme_classic() +
  labs(x = "Global Wealth Score: Standard Deviation",
       title = expression(bold(B.~Model~r^2~vs.~Wealth~Score~Std.~Dev)),
       subtitle = "Performance using all features",
       y = expression(r^2))

p_fb_propfb_scatter <- results_fb_sum_df %>%
  ggplot(aes(y = r2,
             x = prop_pop_on_fb)) +
  geom_smooth(method = lm, se = F, color = "darkorange") +
  geom_point() +
  geom_richtext(aes(label = paste0("Cor = ", round(cor(r2, prop_pop_on_fb),2)),
                    x = 0.4,
                    y = 0.6),
                size = R2_SCATTER_TEXT_SIZE) +
  theme_classic() +
  labs(x = "Proportion of Population on Facebook",
       title = expression(bold(C.~Model~r^2~vs.~Prop.~of~Pop.~on~Facebook)),
       subtitle = "Performance using only Facebook features",
       y = expression(r^2))

p_weather_aggdp_scatter <- results_df %>%
  dplyr::filter(estimation_type %in% "best",
                target_var %in% "pca_allvars",
                feature_type %in% "weatherclimate") %>%
  ggplot(aes(y = r2,
             x = wdi_agric_per_gdp)) +
  geom_smooth(method = lm, se = F, color = "darkorange") +
  geom_point() +
  geom_richtext(aes(label = paste0("Cor = ", round(cor(r2, wdi_agric_per_gdp),2)),
                    x = 10,
                    y = 0.5),
                size = R2_SCATTER_TEXT_SIZE) +
  geom_text_repel(data = . %>% dplyr::filter(wdi_agric_per_gdp >= 30),
                  aes(label = country_name),
                  size = R2_SCATTER_TEXT_SIZE) +
  theme_classic() +
  labs(x = "Agriculture, Forest, and Fishing: % GDP",
       title = expression(bold(D.~Model~r^2~vs.~"%GDP"~Agriculture)),
       subtitle = "Performance using only weather features",
       y = expression(r^2))

# Boxplots across all feature types --------------------------------------------
results_best_df <- results_df %>%
  dplyr::filter(estimation_type %in% "best",
                target_var %in% "pca_allvars") %>%
  dplyr::mutate(feature_type_clean = feature_type_clean %>% fct_rev()) 

## Income GDP Boxplot
p_boxplot_income <- results_best_df %>%
  #dplyr::filter(feature_type_clean != "All Features") %>%
  dplyr::mutate(income = case_when(
    income %in% "Low income" ~ "Low\nincome",
    income %in% "Lower middle income" ~ "Lower middle\nincome",
    income %in% "Upper middle income" ~ "Upper middle\nincome"
  )) %>%
  ggplot(aes(x = r2,
             y = feature_type_clean,
             fill = income)) +
  geom_boxplot() +
  theme_classic() +
  scale_fill_brewer(palette = "Accent",
                    guide = guide_legend(reverse = FALSE)) +
  labs(x = expression(r^2),
       y = NULL,
       title = "D. Model performance by income group across all feature types",
       fill = "Income Group")

## Agriculture GDP Boxplot [For Appendix]
p_boxplot_agdgdp <- results_best_df %>%
  dplyr::mutate(cat = wdi_agric_per_gdp >= 30) %>%
  dplyr::mutate(cat = case_when(
    cat %in% T ~ "True",
    cat %in% F ~ "False"
  )) %>%
  #dplyr::filter(feature_type_clean != "All Features") %>%
  ggplot(aes(x = r2,
             y = feature_type_clean,
             fill = cat)) +
  labs(x = expression(r^2),
       y = NULL,
       fill = ">30% GDP:\nAgriculture\nForestry\nFishing") +
  geom_boxplot() +
  theme_classic() +
  scale_fill_manual(values = c("orange", "limegreen"),
                    guide = guide_legend(reverse = TRUE))

# Arrange ----------------------------------------------------------------------
#p_all_income_box
#p_all_wealthsd_scatter
#p_all_gdp_scatter
#p_fb_propfb_scatter

theme_scatter <- theme(plot.title = element_text(face = "bold", size = 9),
                       plot.subtitle = element_text(size = 8),
                       axis.title.x = element_text(size = 8))

p_l <- ggarrange(p_all_gdp_scatter + 
                   labs(plot.title = "A. Model Performance vs. GDP Per Capita") + 
                   theme_scatter,
                 p_all_wealthsd_scatter + labs(plot.title = "A.") + theme_scatter,
                 p_fb_propfb_scatter + labs(plot.title = "A.") + theme_scatter,
                 #p_weather_aggdp_scatter + labs(plot.title = "A.") + theme_scatter,
                 ncol = 1)

#p_boxplot_income + 
#  theme(legend.position = "bottom",
#        axis.text.y = element_text(color = "black")) +
#  theme_scatter +
#  guides(fill=guide_legend(nrow=3, byrow=T))

# p_r <- ggarrange(p_boxplot_income + 
#                    theme(legend.position = "bottom",
#                          axis.text.y = element_text(color = "black"),
#                          legend.margin=margin(l = -2, unit='cm')) +
#                    theme_scatter +
#                    guides(fill=guide_legend(nrow=1, byrow=T)),
#                  p_wealthsd_income +
#                    theme_scatter +
#                    theme(legend.position = "none",
#                          legend.margin=margin(l = -1, unit='cm')) +
#                    guides(fill=guide_legend(nrow=1, byrow=T)),
#                  ncol = 1,
#                  heights = c(0.75, 0.25))

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
       height = 7,
       width = 9.1)

# Arrange - Extra --------------------------------------------------------------
#p_all_income_box
#p_all_wealthsd_scatter
#p_all_gdp_scatter
#p_fb_propfb_scatter

theme_scatter <- theme(plot.title = element_text(face = "bold", size = 9),
                       plot.subtitle = element_text(size = 8),
                       axis.title.x = element_text(size = 8))

p_l <- ggarrange(p_all_gdp_scatter + 
                   labs(plot.title = "A. Model Performance vs. GDP Per Capita") + 
                   theme_scatter,
                 p_all_wealthsd_scatter + labs(plot.title = "A.") + theme_scatter,
                 p_fb_propfb_scatter + labs(plot.title = "A.") + theme_scatter,
                 p_weather_aggdp_scatter + labs(plot.title = "A.") + theme_scatter,
                 ncol = 1)

#p_boxplot_income + 
#  theme(legend.position = "bottom",
#        axis.text.y = element_text(color = "black")) +
#  theme_scatter +
#  guides(fill=guide_legend(nrow=3, byrow=T))

p_r <- ggarrange(p_boxplot_income + 
                   theme(legend.position = "bottom",
                         axis.text.y = element_text(color = "black"),
                         legend.margin=margin(l = -2, unit='cm')) +
                   theme_scatter +
                   guides(fill=guide_legend(nrow=1, byrow=T)),
                 p_wealthsd_income +
                   theme_scatter +
                   theme(legend.position = "none",
                         legend.margin=margin(l = -1, unit='cm')) +
                   guides(fill=guide_legend(nrow=1, byrow=T)),
                 ncol = 1,
                 heights = c(0.75, 0.25))

p_all <- ggarrange(p_l,
                   p_r,
                   widths = c(0.4, 0.6),
                   nrow = 1)

ggsave(p_all,
       filename = file.path(figures_global_dir, "explain_var_in_r2_extra.png"),
       height = 9,
       width = 9.1)

# Testing ----------------------------------------------------------------------
if(F){
  
  # TESTING =========
  results_df %>%
    dplyr::filter(estimation_type %in% "best",
                  target_var %in% "pca_allvars",
                  feature_type %in% "weather") %>%
    ggplot() +
    geom_point(aes(x = wdi_agric_per_gdp,
                   y = r2))
  
  results_df %>%
    dplyr::filter(estimation_type %in% "best",
                  target_var %in% "pca_allvars",
                  feature_type %in% "all") %>%
    #dplyr::mutate(cat = wdi_agric_per_gdp >= median(wdi_agric_per_gdp)) %>%
    dplyr::mutate(cat = wdi_agric_per_gdp >= 30) %>%
    ggplot() +
    geom_boxplot(aes(x = cat,
                     y = r2))
  
}
