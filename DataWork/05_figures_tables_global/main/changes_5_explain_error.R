# Explain Error

# Load data --------------------------------------------------------------------
changes_df <- readRDS(file.path(data_dir, "DHS", "FinalData", "Merged Datasets", 
                                "survey_alldata_clean_changes_cluster_predictions.Rds"))

y_max <- 7
bb_step <- 0.22
in_plot_txt_size <- 3
in_plot_text_color <- "red"

# Prep data --------------------------------------------------------------------
#### Changes
changes_df <- changes_df %>%
  dplyr::rename(wdi_income = income) %>%
  mutate(error = abs(pca_allvars - predict_pca_allvars_global_country_pred_all_changes),
         error_log = log(error+1),
         error_a2 = error >= 2) %>%
  mutate(urban_rural_yr1 = case_when(
    urban_rural_yr1 == "U" ~ "Urban",
    urban_rural_yr1 == "R" ~ "Rural"
  ) %>% as.factor()) %>%
  mutate(urban_rural_yr2 = case_when(
    urban_rural_yr2 == "U" ~ "Urban",
    urban_rural_yr2 == "R" ~ "Rural"
  ) %>% as.factor())

# Levels -----------------------------------------------------------------------

#### Figures
p_ntl <- changes_df %>%
  ggplot(aes(x = abs(ntlharmon_avg),
             y = error)) +
  geom_point(size = 0.2,
             alpha = 0.2) +
  stat_cor(aes(label = paste(gsub("R", "r", ..rr.label..), ..p.label.., sep = "*`,`~")),
           label.x.npc = "left",
           size = in_plot_txt_size,
           color = in_plot_text_color) +
  geom_smooth(method = lm, se = F, color = "darkorange", size = 0.5) +
  labs(x = "Change in Nighttime Lights, Absolute Value",
       y = "Error",
       title = "Model Error vs Nighttime Lights") +
  theme_classic() 

#### Urban / Rural
med_df <- changes_df %>%
  group_by(urban_rural_yr1) %>%
  summarise(error = median(error) %>% round(2))

p_ur <- changes_df %>%
  ggplot(aes(x = urban_rural_yr1,
             y = error)) +
  geom_boxplot(size = 0.2,
               outlier.size = 0.2) +
  geom_text(data = med_df,
            aes(urban_rural_yr1, error, label = error),
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
med_df <- changes_df %>%
  group_by(wdi_income) %>%
  summarise(error = median(error) %>% round(2))

p_income <- changes_df %>%
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
med_df <- changes_df %>%
  group_by(continent_adj) %>%
  summarise(error = median(error) %>% round(2))

p_continent <- changes_df %>%
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

p_ntl     <- p_ntl + fig_theme
p_ur        <- p_ur + fig_theme
p_continent <- p_continent + fig_theme
p_income    <- p_income + fig_theme

p <- ggarrange(p_ntl, p_ur,
               p_continent, p_income)

ggsave(p,
       filename = file.path(figures_global_dir, "explain_error_changes.png"),
       height = 4,
       width = 8)

# Regression -------------------------------------------------------------------
lm1 <- lm(error ~ abs(ntlharmon_avg) + urban_rural_yr1 + wdi_income + continent_adj, data = changes_df)

stargazer(lm1,
          covariate.labels = c("Change in nighttime lights, absolute value",
                               "Urban (baseline)",
                               "Lower middle income",
                               "Upper middle income",
                               "Americas",
                               "Eurasia"),
          omit.stat = c("f", "ser"),
          dep.var.labels = "Absolute value of difference in change in true and estimated wealth",
          float = F,
          out = file.path(tables_global_dir, "explain_error_changes_lm.tex"))

# Explain Model Error ----------------------------------------------------------
changes_df$error_predict <- predict(lm1, changes_df) %>% as.numeric()

lm(error_predict ~ error, data = changes_df) %>%
  summary()






