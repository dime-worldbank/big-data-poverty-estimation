# DHS / Facebook Eduction Variable Comparson
# Check association of DHS Education Variables with FB Education Variables

# TODO:
# 1. Could check if this correlation works better in countries with more Facebook penetration

library(DescTools)

# Load data --------------------------------------------------------------------
df <- readRDS(file.path(data_dir, "DHS_OLD", "FinalData", "Merged Datasets", "survey_alldata_clean.Rds"))

fb_param_df <- readRDS(file.path(data_dir, "Facebook Marketing", "FinalData", "facebook_marketing_parameters_clean.Rds"))
wdi_df      <- readRDS(file.path(data_dir, "WDI", "FinalData", "wdi.Rds"))
fb_cntry_df <- readRDS(file.path(fb_marketing_dir,  "FinalData", "country_level_mau", "country_level_mau.Rds"))

## Proportion Facebook Coverage
fb_cntry_df <- fb_cntry_df %>%
  dplyr::rename(iso2 = country_iso2) %>%
  left_join(wdi_df, by = "iso2") %>%
  dplyr::mutate(prop_fb_cntry = estimate_mau_1 / wdi_population) %>%
  dplyr::select(iso2, prop_fb_cntry, wdi_population)

df <- df %>%
  left_join(fb_cntry_df, by = "iso2") %>%
  dplyr::filter(most_recent_survey %in% T)

# Prep data --------------------------------------------------------------------
#### Remove NAs
# 
df <- df %>%
  dplyr::mutate(dhs_prop_high_hs = educ_levels_hh_n3_sum/n_hh_members_sum) %>%
  dplyr::mutate(fb_educ_var = fb_prop_estimate_mau_upper_bound_3,
                dhs_educ_var = dhs_prop_high_hs) %>%
  dplyr::filter(!is.na(fb_educ_var),
                !is.na(dhs_educ_var)) 

# #### Panel dataset across unit
#df <- df %>%
#  dplyr::filter(fb_educ_var > 0)

df_sum <- bind_rows(
  df %>%
    dplyr::select(fb_educ_var, dhs_educ_var, country_code, country_name) %>%
    mutate(unit = "Survey Cluster"),
  
  df %>%
    group_by(GID_2, country_code, country_name) %>%
    dplyr::summarise(fb_educ_var = mean(fb_educ_var),
                     dhs_educ_var = mean(dhs_educ_var)) %>%
    ungroup() %>%
    mutate(unit = "ADM 2")
) %>%
  mutate(unit = unit %>%
           factor(levels = c("Survey Cluster",
                             "ADM 2"))) %>%
  group_by(country_code, unit) %>%
  mutate(n = n()) %>%
  ungroup() 

## Use constant set of countries where enough ADM 2 observations.
countries_to_use <- df_sum %>%
  dplyr::filter(unit == "ADM 2",
                n >= 30) %>%
  pull(country_code) %>%
  unique()

df_sum <- df_sum[df_sum$country_code %in% countries_to_use,]

## Must have some non-0 DHS and FB observations
countries_to_use <- df_sum %>%
  dplyr::filter(unit == "Survey Cluster") %>%
  group_by(country_code) %>%
  dplyr::summarise(dhs_educ_var_sum = sum(dhs_educ_var),
                   fb_educ_var_sum = sum(fb_educ_var)) %>%
  ungroup() %>%
  dplyr::filter(dhs_educ_var_sum > 0,
                fb_educ_var_sum > 0) %>%
  pull(country_code) %>%
  unique()

df_sum <- df_sum[df_sum$country_code %in% countries_to_use,]

# Correlation Dataset ----------------------------------------------------------
df_cor <- df_sum %>%
  group_by(unit, country_code, country_name) %>%
  dplyr::summarise(cor = cor(fb_educ_var, dhs_educ_var),
                   n = n()) %>%
  ungroup()

# Figure: Boxplot --------------------------------------------------------------
p <- df_cor %>%
  ggplot(aes(x = unit,
             y = cor)) +
  geom_half_boxplot(errorbar.draw = FALSE, center = TRUE, 
                    fill = "gray80") +
  stat_summary(fun = median, geom = "text", col = "black",     
               vjust = -0.2, aes(label = paste(round(..y.., digits = 2)))) +
  stat_summary(fun = max, geom = "text", col = "firebrick3",    
               vjust = -0.2, aes(label = paste(round(..y.., digits = 2)))) +
  labs(x = NULL,
       y = "Within Country Correlation Correlation") +
  scale_y_continuous(limits = c(0,1)) +
  theme_classic() +
  theme(legend.position = "none",
        axis.text.y = element_text(face = "bold")) +
  coord_flip() 

ggsave(p, 
       filename = file.path(figures_global_dir, "educ_fb_dhs_boxplot.png"),
       height = 4,
       width = 6)

# Figure: Scatter --------------------------------------------------------------
df_sum <- df_sum %>%
  group_by(unit, country_code, country_name) %>%
  dplyr::mutate(cor = cor(fb_educ_var, dhs_educ_var)) %>%
  ungroup() %>%
  dplyr::mutate(title = paste0(country_name, "\nCor = ", round(cor, 2))) %>%
  dplyr::mutate(title = reorder(title, cor) %>% fct_rev())

## Cluster
p <- df_sum %>%
  dplyr::filter(unit == "Survey Cluster") %>%
  ggplot(aes(x = fb_educ_var,
             y = dhs_educ_var)) +
  geom_point(size = 0.75) +
  theme_minimal() +
  theme(strip.text = element_text(face = "bold")) +
  labs(x = "Proportion Above High School Education [Facebook]",
       y = "Proportion Above High School Education [DHS]") +
  facet_wrap(~title,
             scales = "free")

ggsave(p, 
       filename = file.path(figures_global_dir, "educ_fb_dhs_scatter_cluster.png"),
       height = 12,
       width = 12)

## ADM2
p <- df_sum %>%
  dplyr::filter(unit == "ADM 2") %>%
  ggplot(aes(x = fb_educ_var,
             y = dhs_educ_var)) +
  geom_point(size = 0.75) +
  theme_minimal() +
  theme(strip.text = element_text(face = "bold")) +
  labs(x = "Proportion Above High School Education [Facebook]",
       y = "Proportion Above High School Education [DHS]") +
  facet_wrap(~title,
             scales = "free")

ggsave(p, 
       filename = file.path(figures_global_dir, "educ_fb_dhs_scatter_adm2.png"),
       height = 12,
       width = 12)

# Explain Correlation ----------------------------------------------------------
df_country_vars <- df %>%
  distinct(country_code, prop_fb_cntry, wdi_population)

df_cor_data <- df_cor %>%
  left_join(df_country_vars, by = "country_code") %>%
  mutate(wdi_population_log = log(wdi_population),
         n = Winsorize(n)) %>%
  pivot_longer(cols = -c(unit, country_code, country_name, cor)) %>%
  dplyr::filter(name %in% c("prop_fb_cntry", 
                            "wdi_population_log",
                            "n")) %>%
  dplyr::mutate(name = case_when(
    name == "prop_fb_cntry" ~ "Prop. on Facebook",
    name == "wdi_population_log" ~ "Population, Logged",
    name == "n" ~ "N Units"
  ))

p <- df_cor_data %>%
  ggplot(aes(x = cor,
             y = value)) +
  stat_poly_line(color = "darkorange",
                 se = F) +
  geom_point() +
  stat_poly_eq(small.r = T, color = "firebrick2") +
  # stat_poly_eq(aes(label = paste(after_stat(eq.label),
  #                                after_stat(rr.label), sep = "*\", \"*"))) +
  #theme_classic2() +
  theme(strip.text = element_text(face = "bold")) +
  labs(x = "Within Country Correlation",
       y = "Value") +
  facet_grid(name~unit,
             scales = "free")

ggsave(p, 
       filename = file.path(figures_global_dir, "educ_fb_dhs_explain.png"),
       height = 6,
       width = 6)
