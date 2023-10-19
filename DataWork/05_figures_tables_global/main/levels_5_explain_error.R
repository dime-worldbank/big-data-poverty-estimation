# Explain Error

# Load data --------------------------------------------------------------------
level_df <- readRDS(file.path(data_dir, "DHS", "FinalData", "Merged Datasets", 
                              "survey_alldata_clean_predictions.Rds"))

wdi_df <- readRDS(file.path(data_dir, "WDI", "FinalData", "wdi.Rds"))

y_max <- 10
bb_step <- 0.22
in_plot_txt_size <- 3
in_plot_text_color <- "red"

# Prep data --------------------------------------------------------------------
#### WDI
wdi_df <- wdi_df %>%
  dplyr::select(iso2, wdi_population, wdi_gdp_pc, wdi_agric_per_gdp, income) %>%
  dplyr::rename(wdi_income = income)

#### Level
level_df <- level_df %>%
  dplyr::filter(most_recent_survey %in% T) %>%
  left_join(wdi_df, by = "iso2") %>%
  mutate(error = abs(pca_allvars_mr - predict_pca_allvars_mr_global_country_pred_all),
         error_log = log(error+1),
         error_a2 = error >= 2) %>%
  mutate(urban_rural = case_when(
    urban_rural == "U" ~ "Urban",
    urban_rural == "R" ~ "Rural"
  ) %>% as.factor())

# Figures ----------------------------------------------------------------------
p_viirs <- level_df %>%
  ggplot(aes(x = viirs_avg_rad,
             y = error)) +
  geom_point(size = 0.2,
             alpha = 0.2) +
  stat_cor(aes(label = paste(gsub("R", "r", ..rr.label..), ..p.label.., sep = "*`,`~")),
           label.x.npc = "left",
           size = in_plot_txt_size,
           color = in_plot_text_color) +
  geom_smooth(method = lm, se = F, color = "darkorange", size = 0.5) +
  labs(x = "Average Nighttime Lights",
       y = "Error",
       title = "Model Error vs Nighttime Lights") +
  theme_classic() 

#### Urban / Rural
med_df <- level_df %>%
  group_by(urban_rural) %>%
  summarise(error = median(error) %>% round(2))

p_ur <- level_df %>%
  ggplot(aes(x = urban_rural,
             y = error)) +
  geom_boxplot(size = 0.2,
               outlier.size = 0.2) +
  geom_text(data = med_df,
            aes(urban_rural, error, label = error),
            color = in_plot_text_color,
            position = position_dodge(width = 0.8), size = in_plot_txt_size, vjust = -0.2) +
  geom_signif(comparisons = list(c("Urban", "Rural")), 
              textsize = 3,
              map_signif_level=TRUE) +
  labs(x = NULL,
       y = "Error",
       title = "Model Error vs Urban or Rural") +
  theme_classic() +
  scale_y_continuous(limits = c(0, y_max)) 

#### Income
med_df <- level_df %>%
  group_by(wdi_income) %>%
  summarise(error = median(error) %>% round(2))

p_income <- level_df %>%
  ggplot(aes(x = wdi_income,
             y = error)) +
  geom_boxplot(size = 0.2,
               outlier.size = 0.2) +
  geom_text(data = med_df,
            aes(wdi_income, error, label = error),
            color = in_plot_text_color,
            position = position_dodge(width = 0.8), size = in_plot_txt_size, vjust = -0.2) +
  geom_signif(comparisons = list(c("Low income", 
                                   "Lower middle income"),
                                 c("Low income", 
                                   "Upper middle income"),
                                 c("Lower middle income", 
                                   "Upper middle income")), 
              map_signif_level=T,
              textsize = 3,
              step_increase = bb_step) +
  labs(x = NULL,
       y = "Error",
       title = "Model Error vs Country Income Level") +
  theme_classic() +
  scale_y_continuous(limits = c(0, y_max))

#### Continent
med_df <- level_df %>%
  group_by(continent_adj) %>%
  summarise(error = median(error) %>% round(2))

p_continent <- level_df %>%
  ggplot(aes(x = continent_adj,
             y = error)) +
  geom_boxplot(size = 0.2,
               outlier.size = 0.2) +
  geom_text(data = med_df,
            aes(continent_adj, error, label = error),
            color = in_plot_text_color,
            position = position_dodge(width = 0.8), size = in_plot_txt_size, vjust = -0.2) +
  geom_signif(comparisons = list(c("Africa", 
                                   "Americas"),
                                 c("Africa", 
                                   "Eurasia"),
                                 c("Americas", 
                                   "Eurasia")), 
              map_signif_level=T,
              textsize = 3,
              step_increase = bb_step) +
  labs(x = NULL,
       y = "Error",
       title = "Model Error vs Continent") +
  theme_classic() +
  scale_y_continuous(limits = c(0, y_max))

# Arrange ----------------------------------------------------------------------
fig_theme <- theme(plot.title = element_text(face = "bold", size = 9),
                   plot.subtitle = element_text(size = 8),
                   axis.title.x = element_text(size = 8),
                   axis.title.y = element_text(size = 8),
                   axis.text.x = element_text(size = 8, color = "black"),
                   axis.text.y = element_text(size = 8, color = "black"))

p_viirs     <- p_viirs + fig_theme
p_ur        <- p_ur + fig_theme
p_continent <- p_continent + fig_theme
p_income    <- p_income + fig_theme

p <- ggarrange(p_viirs, p_ur,
               p_continent, p_income)

ggsave(p,
       filename = file.path(figures_global_dir, "explain_error_levels.png"),
       height = 4,
       width = 8)

# Regression -------------------------------------------------------------------
lm1 <- lm(error ~ viirs_avg_rad + urban_rural + wdi_income + continent_adj, data = level_df)

stargazer(lm1,
          covariate.labels = c("Nighttime lights",
                               "Urban",
                               "Lower middle income",
                               "Upper middle income",
                               "Americas",
                               "Eurasia"),
          dep.var.labels = "Absolute value of difference in true and predicted wealth",
          omit.stat = c("f", "ser"),
          float = F,
          out = file.path(tables_global_dir, "explain_error_levels_lm.tex"))

# Explain Model Error ----------------------------------------------------------
level_df$error_predict <- predict(lm1, level_df) %>% as.numeric()

lm(error_predict ~ error, data = level_df) %>%
  summary()








