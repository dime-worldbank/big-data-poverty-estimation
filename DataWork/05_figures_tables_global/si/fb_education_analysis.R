# DHS / Facebook Eduction Variable Comparson
# Check association of DHS Education Variables with FB Education Variables

# TODO:
# 1. Could check if this correlation works better in countries with more Facebook penetration

# Load data --------------------------------------------------------------------
df <- readRDS(file.path(data_dir, "DHS", "FinalData", "Merged Datasets", "survey_alldata_clean.Rds"))

fb_param_df <- readRDS(file.path(data_dir, "Facebook Marketing", "FinalData", "facebook_marketing_parameters_clean.Rds"))
wdi_df <- readRDS(file.path(data_dir, "WDI", "FinalData", "wdi.Rds"))
fb_cntry_df <- readRDS(file.path(fb_marketing_dir,  "FinalData", "country_level_mau", "country_level_mau.Rds"))

## Proportion Facebook Coverage
fb_cntry_df <- fb_cntry_df %>%
  dplyr::rename(iso2 = country_iso2) %>%
  left_join(wdi_df, by = "iso2") %>%
  dplyr::mutate(prop_fb_cntry = estimate_mau_1 / wdi_population) %>%
  dplyr::select(iso2, prop_fb_cntry, wdi_population)

df <- df %>%
  left_join(fb_cntry_df, by = "iso2")

# Figures ----------------------------------------------------------------------
#### Remove NAs
df <- df %>%
  dplyr::mutate(fb_educ_var = fb_prop_estimate_mau_upper_bound_3,
                dhs_educ_var = educ_years_hh_max) %>%
  dplyr::filter(!is.na(fb_educ_var),
                !is.na(dhs_educ_var)) 

#### Global Scatterplot
df_sum <- bind_rows(
  df %>%
    dplyr::select(fb_educ_var, dhs_educ_var) %>%
    mutate(level = "Survey Cluster"),
  
  df %>%
    group_by(country_code) %>%
    dplyr::summarise_at(vars(fb_educ_var, dhs_educ_var), mean) %>%
    ungroup() %>%
    mutate(level = "Country"),
  
  df %>%
    group_by(GID_1) %>%
    dplyr::summarise_at(vars(fb_educ_var, dhs_educ_var), mean) %>%
    ungroup() %>%
    mutate(level = "ADM 1"),
  
  df %>%
    group_by(GID_2) %>%
    dplyr::summarise_at(vars(fb_educ_var, dhs_educ_var), mean) %>%
    ungroup() %>%
    mutate(level = "ADM 2")
) %>%
  mutate(level = level %>%
           factor(levels = c("Survey Cluster",
                             "ADM 2",
                             "ADM 1",
                             "Country")))

p_cor <- df_sum %>%
  ggplot(aes(x = fb_educ_var,
             y = dhs_educ_var)) +
  geom_point() +
  stat_cor(aes(label = ..r.label..),
           label.x.npc = "left",
           label.y.npc = "top",
           color = "red") +
  labs(title = "A. Association between education variables from DHS and Facebook aggregated to different units",
       x = "Proportion More than High School Education [Facebook]",
       y = "Years of\nEducation,\nMaximum\nin Household\n[DHS]") +
  theme_classic() +
  theme(strip.text = element_text(face = "bold"),
        plot.title = element_text(face = "bold", hjust = 0),
        strip.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(angle = 0, vjust = 0.5)) +
  facet_wrap(~level,
             scales = "free",
             nrow = 1)

#### Within Country Correlation
df_cor <- bind_rows(
  df %>%
    group_by(country_code) %>%
    dplyr::summarise(cor = cor(fb_educ_var, dhs_educ_var),
                     prop_fb_cntry = mean(prop_fb_cntry),
                     wdi_population = mean(wdi_population),
                     N = n()) %>%
    mutate(level = "Survey Cluster"),
  
  df %>%
    group_by(country_code, GID_2) %>%
    dplyr::summarise(fb_educ_var = mean(fb_educ_var),
                     dhs_educ_var = mean(dhs_educ_var),
                     prop_fb_cntry = mean(prop_fb_cntry),
                     wdi_population = mean(wdi_population)) %>%
    ungroup() %>%
    group_by(country_code) %>%
    dplyr::summarise(cor = cor(fb_educ_var, dhs_educ_var),
                     prop_fb_cntry = mean(prop_fb_cntry),
                     wdi_population = mean(wdi_population),
                     N = n()) %>%
    dplyr::filter(N >= 20) %>%
    mutate(level = "ADM 2"),
  
  df %>%
    group_by(country_code, GID_1) %>%
    dplyr::summarise(fb_educ_var = mean(fb_educ_var),
                     dhs_educ_var = mean(dhs_educ_var),
                     prop_fb_cntry = mean(prop_fb_cntry),
                     wdi_population = mean(wdi_population)) %>%
    ungroup() %>%
    group_by(country_code) %>%
    dplyr::summarise(cor = cor(fb_educ_var, dhs_educ_var),
                     prop_fb_cntry = mean(prop_fb_cntry),
                     wdi_population = mean(wdi_population),
                     N = n()) %>%
    dplyr::filter(N >= 10) %>%
    mutate(level = "ADM 1")
) %>%
  mutate(level = level %>%
           factor(levels = c("Survey Cluster",
                             "ADM 2",
                             "ADM 1")))

## N countries
df_cor %>%
  group_by(level) %>%
  dplyr::summarise(N = n())

## Distribution of within country correlation
p_dist_within_cor <- df_cor %>%
  ggplot(aes(x = cor,
             y = level)) +
  geom_boxplot(fill = "gray80") +
  theme_classic() +
  theme(plot.title = element_text(face = "bold"),
        axis.text.y = element_text(face = "bold", color = "black")) +
  labs(title = "B. Distribution of within\ncountry correlation\nbetween DHS and\nFacebook education\nvariables",
       y = NULL,
       x = "Within Country Correlation")

## Assocation of within country correlation and Facebook usage
p_prop_fb_cor <- df_cor %>%
  ggplot(aes(x = cor,
             y = prop_fb_cntry)) + 
  geom_point() +
  stat_cor(aes(label = ..r.label..),
           label.y.npc = "top",
           color = "red") +
  theme_classic() +
  theme(strip.text = element_text(face = "bold"),
        plot.title = element_text(face = "bold", hjust = 0),
        strip.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(angle = 0, vjust = 0.5)) +
  labs(title = "C. Association between (1) within country correlation of DHS and Facebook\neducation variables and (2) proportion of country that uses Facebook\nacross difference levels of aggregation",,
       x = "Within Country Correlation",
       y = "Proportion\nof Country\nUses\nFacebook") +
  facet_wrap(~level,
             scales = "free")

# Arrange and export -----------------------------------------------------------

p_within <- ggarrange(p_dist_within_cor,
                      p_prop_fb_cor,
                      widths = c(0.3, 0.7))

p <- ggarrange(p_cor,
               p_within,
               ncol = 1)

ggsave(p, 
       filename = file.path(figures_global_dir, "educ_fb_dhs.png"),
       height = 7,
       width = 11)










