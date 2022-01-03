# Correlations of Features with Poverty

# Products tables/figures of (1) all features and (2) select features

# Within country; show distribution. (color by continent?)

# Load data --------------------------------------------------------------------
df <- readRDS(file.path(data_dir, SURVEY_NAME, "FinalData", "Merged Datasets", "survey_alldata_clean.Rds"))

fb_param_df <- readRDS(file.path(data_dir, "Facebook Marketing", "FinalData", "facebook_marketing_parameters_clean.Rds"))
gc_param_df <- read_csv(file.path(data_dir, "Globcover", "RawData", "gc_classes.csv"))

# Compute correlation ----------------------------------------------------------
# Compute correlation for each variable & country

### Data to long to make easier to compute correlation across countries + variables
df_long <- df %>%
  dplyr::select(country_name, 
                continent_adj,
                pca_allvars,
                starts_with("viirs_"),
                starts_with("gc_"),
                starts_with("osm_"),
                starts_with("l8_"),
                starts_with("cnn_"),
                starts_with("fb_prop"),
                starts_with("worldclim_"),
                starts_with("worldpop_"),
                starts_with("elevslope_"),
                starts_with("globalmod_"),
                starts_with("pollution_aod_"),
                starts_with("pollution_s5p_"),
                starts_with("weather_")) %>%
  pivot_longer(cols = -c(country_name, pca_allvars, continent_adj)) %>%
  dplyr::rename(variable = name) %>%
  dplyr::mutate(variable_cat = case_when(
    variable %>% str_detect("viirs_") ~ "Nighttime Lights",
    variable %>% str_detect("gc_") ~ "Land Cover",
    variable %>% str_detect("osm_") ~ "OpenStreetMap",
    variable %>% str_detect("l8_") ~ "Daytime Imagery, Average",
    variable %>% str_detect("cnn_s2_rgb_") ~ "Daytime Imagery, CNN: RGB",
    variable %>% str_detect("cnn_s2_ndvi_") ~ "Daytime Imagery, CNN: NDVI",
    variable %>% str_detect("cnn_s2_bu_") ~ "Daytime Imagery, CNN: BU",
    variable %>% str_detect("fb_prop") ~ "Facebook Marketing",
    variable %>% str_detect("worldclim") ~ "World Climate",
    variable %>% str_detect("worldpop") ~ "World Pop",
    variable %>% str_detect("elevslope") ~ "Elevation/Slope",
    variable %>% str_detect("globalmod") ~ "Global Human Mod Index",
    variable %>% str_detect("pollution_aod") ~ "Pollution - MODIS",
    variable %>% str_detect("pollution_s5p") ~ "Pollution - Sentinel-5P",
    variable %>% str_detect("weather") ~ "Weather",
    TRUE ~ variable
  ))

### Compute Correlation
cor_df <- df_long %>%
  dplyr::filter(!is.na(pca_allvars),
                !is.na(value)) %>%
  group_by(variable, variable_cat, country_name, continent_adj) %>%
  dplyr::summarise(cor = cor(pca_allvars, value)) %>%
  ungroup() %>%
  
  # TODO: CHECK WHY THIS WOULD HAPPEN
  dplyr::filter(!is.na(cor))

### Clean select name for variables
## Facebook
fb_param_clean_df <- fb_param_df %>%
  dplyr::select(param_id, param_category, param_name_simple) %>%
  dplyr::mutate(variable = paste0("fb_prop_estimate_mau_upper_bound_", param_id),
                variable_clean_fb = paste0(param_category, ": ", param_name_simple)) %>%
  dplyr::select(variable, variable_clean_fb)

## Globcover
gc_param_clean_df <- gc_param_df %>%
  dplyr::select(value, label) %>%
  dplyr::rename(variable = value,
                variable_clean_gc = label) %>%
  dplyr::mutate(variable = paste0("gc_", variable))

cor_df <- cor_df %>%
  left_join(fb_param_clean_df, by = "variable") %>%
  left_join(gc_param_clean_df, by = "variable") %>%
  dplyr::mutate(variable_clean = case_when(
    variable_cat %>% str_detect("Facebook") ~ variable_clean_fb,
    variable_cat %>% str_detect("Land Cover") ~ variable_clean_gc,
    TRUE ~ variable
  )) %>%
  dplyr::select(-c(variable_clean_fb, variable_clean_gc))

# Correlations: All ------------------------------------------------------------
# TODO: Could do top 5, 3 or just top 1.
# Show correlations of top 5 in each category

### Variables with top 5 correlation within each category, using median correlation
### across countries
cor_median_topvars_df <- cor_df %>% 
  # Median across countries
  group_by(variable, variable_cat) %>%
  dplyr::summarise(cor = median(cor)) %>%
  ungroup() %>%
  
  # Top 5 correlations by category
  group_by(variable_cat) %>% 
  slice_max(order_by = cor, n = 1)

### Subset country-level correlation data
cor_topvars_df <- cor_df[cor_df$variable %in% cor_median_topvars_df$variable,]

### Cleanup Names
cor_topvars_clean_df <- cor_topvars_df %>%
  arrange(-cor) %>%
  clean_varnames()
# dplyr::mutate(variable_clean = case_when(
#   is.na(variable_clean) ~ variable,
#   TRUE ~ variable_clean
# )) %>%
# dplyr::mutate(variable_clean = case_when(
#   variable_clean %in% "viirs_avg_rad" ~ "[VIIRS] Nighttime Lights",
#   variable_clean %in% "gc_190" ~ "[Globcover] Urban Land",
#   variable_clean %in% "globalmod_mean" ~ "[Global Human Modification Index]",
#   variable_clean %in% "osm_length_residential_5000m" ~ "[OSM] Residential Road Length",
#   variable_clean %in% "cnn_s2_rgb_pc11" ~ "[Daytime Imagery, CNN: RGB] Feature 1",
#   variable_clean %in% "cnn_s2_ndvi_pc7" ~ "[Daytime Imagery, CNN: NDVI] Feature 1",
#   variable_clean %in% "cnn_s2_bu_pc16"  ~ "[Daytime Imagery, CNN: Built-Up Index] Feature 1",
#   variable_clean %in% "worldpop_10km" ~ "[World Pop] Population",
#   variable_clean %in% "pollution_s5p_tropospheric_NO2_column_number_density" ~ 
#     "[Pollution, Sentinel-5P] Tropospheric NO2",
#   variable_clean %in% "l8_B2" ~ "[Landsat 8] Blue Surface Reflectance (B2)",
#   variable_clean %in% "Interests: Restaurants" ~ "[Facebook] Interests: Restaurants",
#   variable_clean %in% "weather_q4_minimum_2m_air_temperature" ~ "[Weather] Quarter 4 Min. Temperature",
#   variable_clean %in% "pollution_aod_047" ~ "[Pollution, MODIS] AOD",
#   variable_clean %in% "worldclim_bio_6" ~ "[WorldClim] Bio 6",
#   variable_clean %in% "elevslope_slope" ~ "[Elevation/Slope] Slope",
#   TRUE ~ variable_clean
# ))

### Figure
p_topvar <- cor_topvars_clean_df %>%
  ggplot() +
  geom_vline(xintercept = 0, alpha = 0.5) +
  geom_boxplot(aes(x = cor,
                   y = reorder(variable_clean_with_dataset, cor, FUN = median, .desc =TRUE)),
               fill = "gray80") +
  labs(title = "A. Correlation of Select Variables to Wealth Score Across Countries",
       subtitle = "The variable with the highest median correlation for each dataset is shown",
       y = NULL,
       x = "Correlation") + 
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10),
                     limits = c(min(cor_topvars_clean_df$cor),
                                1)) +
  theme_classic() +
  theme(plot.title = element_text(face = "bold", size = 10),
        axis.text.y = element_text(color = "black"),
        plot.title.position = "plot")

ggsave(p_topvar,
       filename = file.path(figures_global_dir, "cor_topvar_category.png"),
       height = 5,
       width = 8.5)

# CORRELATIONS BY DATASET ------------------------------------------------------

# ** Facebook ------------------------------------------------------------------
p_fb <- cor_df %>%
  dplyr::filter(variable_cat %in% "Facebook Marketing") %>%
  dplyr::mutate(var_indv = variable_clean %>% 
                  str_replace_all(".*:", "") %>% 
                  str_squish(),
                var_cat = variable_clean %>% 
                  str_replace_all(":.*", "")) %>%
  dplyr::mutate(var_indv = case_when(
    var_indv == "Samsung Galaxy phone S8/S8+/S9/S9+ or Apple iPhone X/XS/XS Max/XR" ~ 
      "Samsung Galaxy phone S8+ or Apple iPhone X+",
    TRUE ~ var_indv
  )) %>%
  ggplot() +
  geom_vline(xintercept = 0, alpha = 0.1) +
  geom_vline(xintercept = -0.5, alpha = 0.1) +
  geom_vline(xintercept = 0.5, alpha = 0.1) +
  geom_vline(xintercept = 1, alpha = 0.1) +
  geom_boxplot(aes(x = cor,
                   y = reorder(var_indv, cor, FUN = mean, .desc =TRUE),
                   fill = var_cat)) +
  labs(y = NULL,
       x = "Correlation",
       fill = "Variable\nCategory",
       title = "B. Correlation of Facebook Variables to Wealth Score") +
  theme_classic() +
  scale_x_continuous(limits = c(-0.5, 1)) +
  theme(axis.text.y = element_text(color = "black"),
        plot.title = element_text(face = "bold", size = 10),
        legend.position = "bottom",
        legend.margin = margin(t = 0, r = 0, b = 0, l = -200, unit = "pt"),
        plot.title.position = "plot") +
  scale_fill_brewer(palette = "Accent") +
  guides(fill=guide_legend(nrow=3,byrow=T))

ggsave(p_fb,
       filename = file.path(figures_global_dir, "cor_fb.png"),
       height = 6,
       width = 9.5)

# ** OpenStreetMap -------------------------------------------------------------
## Use top 20 variables
osm_vars_to_use <- cor_df %>%
  dplyr::filter(variable_cat %in% "OpenStreetMap") %>%
  group_by(variable) %>%
  dplyr::summarise(cor = median(cor)) %>%
  slice_max(order_by = cor, n = 20) %>%
  pull(variable)

p_osm <- cor_df %>%
  dplyr::filter(variable %in% all_of(osm_vars_to_use)) %>%
  dplyr::mutate(osm_cat = case_when(
    variable_clean %>% str_detect("osm_length_") ~ "Road Length",
    variable_clean %>% str_detect("osm_n_poi_") ~ "N POI",
    variable_clean %>% str_detect("osm_distmeters_poi") ~ "Min Distance POI",
    variable_clean %>% str_detect("osm_distmeters_road") ~ "Min Distance Road"
  )) %>%
  dplyr::mutate(variable_clean = variable_clean %>%
                  str_replace_all("osm_distmeters_poi_any", "osm_distmeters_poi_any_poi") %>%
                  str_replace_all("osm_distmeters_road_any", "osm_distmeters_road_any_road") %>%
                  str_replace_all("osm_length_", "Length: ") %>%
                  str_replace_all("osm_n_poi_", "N POI: ") %>%
                  str_replace_all("osm_distmeters_poi_", "Min. Dist: ") %>%
                  str_replace_all("osm_distmeters_road_", "Min. Dist: ") %>%
                  str_replace_all("_5000m", "") %>%
                  str_replace_all("_buff", "") %>%
                  str_replace_all("_", " ") %>%
                  tools::toTitleCase() %>%
                  str_replace_all("\\ball\\b", "All") %>%
                  str_replace_all("\\bany\\b", "Any") %>%
                  str_replace_all(".*:", "") %>% 
                  str_squish()) %>%
  dplyr::filter(osm_cat %in% c("Road Length",
                               "N POI")) %>%
  ggplot() +
  geom_vline(xintercept = 0, alpha = 0.1) +
  geom_vline(xintercept = -0.5, alpha = 0.1) +
  geom_vline(xintercept = 0.5, alpha = 0.1) +
  geom_vline(xintercept = 1, alpha = 0.1) +
  geom_boxplot(aes(x = cor,
                   y = reorder(variable_clean, cor, FUN = mean, .desc =TRUE),
                   fill = osm_cat)) +
  labs(y = NULL,
       x = "Correlation",
       fill = "Variable\nCategory",
       title = "C. Correlation of Select OSM Variables to Wealth Score") +
  scale_x_continuous(limits = c(-0.5, 1)) +
  theme_classic() +
  theme(axis.text.y = element_text(color = "black"),
        plot.title = element_text(face = "bold", size = 10),
        legend.position = "bottom",
        plot.title.position = "plot") + 
  scale_fill_manual(values = c("cadetblue2", "tan1"))

ggsave(p_osm,
       filename = file.path(figures_global_dir, "cor_osm.png"),
       height = 10,
       width = 8.5)

# ** Pollution -----------------------------------------------------------------
p_pollution <- cor_df %>%
  dplyr::filter(variable_cat %in% c("Pollution - MODIS",
                                    "Pollution - Sentinel-5P")) %>%
  clean_varnames() %>%
  ggplot() +
  geom_vline(xintercept = 0, alpha = 0.1) +
  geom_vline(xintercept = -0.5, alpha = 0.1) +
  geom_vline(xintercept = 0.5, alpha = 0.1) +
  geom_vline(xintercept = 1, alpha = 0.1) +
  geom_boxplot(aes(x = cor,
                   y = reorder(variable_clean, cor, FUN = mean, .desc =TRUE)),
               fill = "darkseagreen3") +
  labs(y = NULL,
       x = "Correlation",
       fill = "Variable\nCategory",
       title = "D. Correlation of Pollution Variables to Wealth Score") +
  scale_x_continuous(limits = c(-0.539, 1)) +
  theme_classic() +
  theme(axis.text.y = element_text(color = "black"),
        plot.title = element_text(face = "bold", size = 10),
        plot.title.position = "plot") 

# ** Globcover -----------------------------------------------------------------
p_gc <- cor_df %>%
  dplyr::filter(variable_cat %in% "Land Cover") %>%
  ggplot() +
  geom_vline(xintercept = 0, alpha = 0.5) +
  geom_boxplot(aes(x = cor,
                   y = reorder(variable_clean, cor, FUN = mean, .desc =TRUE)),
               fill = "orange") +
  labs(y = NULL,
       x = "Correlation",
       title = "Correlation of Globcover Variables to Asset Index") +
  theme_classic() +
  theme(axis.text.y = element_text(color = "black"),
        plot.title = element_text(face = "bold"),
        plot.title.position = "plot")

ggsave(p_gc,
       filename = file.path(figures_global_dir, "cor_globcover.png"),
       height = 6,
       width = 9.5)

# ** World Clim ----------------------------------------------------------------
p_worldclim <- cor_df %>%
  dplyr::filter(variable_cat %in% "World Climate") %>%
  clean_varnames() %>%
  ggplot() +
  geom_vline(xintercept = 0, alpha = 0.5) +
  geom_boxplot(aes(x = cor,
                   y = reorder(variable_clean, cor, FUN = mean, .desc =TRUE)),
               fill = "lightsteelblue1") +
  labs(y = NULL,
       x = "Correlation",
       title = "Correlation of WorldClim Bioclimatic Variables to Wealth Index") +
  theme_classic() +
  theme(axis.text.y = element_text(color = "black"),
        plot.title = element_text(face = "bold"),
        plot.title.position = "plot")

ggsave(p_worldclim,
       filename = file.path(figures_global_dir, "cor_worldclim.png"),
       height = 6,
       width = 9.5)

# Arrange ----------------------------------------------------------------------
p_osm_pollution <- ggarrange(p_osm,
                             p_pollution,
                             nrow = 2,
                             heights = c(0.7, 0.3))

p_indv <- ggarrange(p_fb, 
                    p_osm_pollution,
                    widths = c(0.55, 0.45),
                    nrow = 1)

p <- ggarrange(p_topvar,
               p_indv,
               nrow = 2,
               heights = c(0.32, 0.68))

ggsave(p,
       filename = file.path(figures_global_dir, "cor_all_fb_osm.png"),
       height = 11,
       width = 9)

# CORRELATION BETWEEN VARIABLES ------------------------------------------------
#### Prep Data
sub_df <- df %>%
  dplyr::select(country_code, all_of(cor_median_topvars_df$variable))

cor_var_df <- map_df(unique(sub_df$country_code), function(country_code_i){
  sub_df %>%
    dplyr::filter(country_code %in% all_of(country_code_i)) %>%
    dplyr::select(-country_code) %>%
    cor() %>%
    reshape2::melt() %>%
    dplyr::mutate(country_code = country_code_i)
  
})

## Merge with correlation with wealth score for ordering
cor_var_sumstat_df <- cor_var_df %>%
  group_by(Var1, Var2) %>%
  dplyr::summarise(cor_mean = mean(value, na.rm = T),
                   cor_sd = sd(value, na.rm = T))

cor_median_topvars_clean_df <- cor_median_topvars_df %>%
  dplyr::select(variable, cor) %>%
  dplyr::rename(cor_wealth_score = cor) %>%
  clean_varnames()

cor_var_sumstat_df <- cor_var_sumstat_df %>%
  merge(cor_median_topvars_clean_df, by.x = "Var1", by.y = "variable") %>%
  merge(cor_median_topvars_clean_df, by.x = "Var2", by.y = "variable")

#### Mean Figure
p_cor_mean <- cor_var_sumstat_df %>%
  ggplot(aes(x = reorder(variable_clean_with_dataset.x,  cor_wealth_score.x),
             y = reorder(variable_clean_with_dataset.y,  cor_wealth_score.y),
             fill = cor_mean)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(cor_mean, 2)),
            size = 3.5) +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       na.value = "white",
                       name="Correlation") +
  scale_y_discrete(position = "right") +
  theme_minimal()+ 
  theme(axis.text.y.right = element_text(hjust = 0.5,
                                         color = "black",
                                         size = 12),
        axis.text.x.bottom = element_text(angle = 45, 
                                          hjust = 1,
                                          color = "black",
                                          size = 12),
        
        legend.position = "top",
        plot.title = element_text(face = "bold", size = 13)) +
  coord_fixed() +
  labs(x = NULL,
       y = NULL,
       title = "A. Average Correlation Between Variables Across Countries")

#### Standard Deviation Figure
p_cor_sd <- cor_var_sumstat_df %>%
  ggplot(aes(x = reorder(variable_clean_with_dataset.x,  cor_wealth_score.x),
             y = reorder(variable_clean_with_dataset.y,  cor_wealth_score.y),
             fill = cor_sd)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(cor_sd, 2)),
            color = "white",
            size = 3.5) +
  #scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
  #                     midpoint = 0, limit = c(0,0.4), space = "Lab", 
  #                     na.value = "white",
  #                     name="Correlation") +
  scale_fill_viridis() +
  scale_y_discrete(position = "right") +
  theme_minimal()+ 
  theme(axis.text.y.right = element_blank(),
        axis.text.x.bottom = element_text(angle = 45, 
                                          hjust = 1,
                                          color = "black",
                                          size = 12),
        
        legend.position = "top",
        plot.title = element_text(face = "bold", size = 13)) +
  coord_fixed() +
  labs(x = NULL,
       y = NULL,
       fill = "Std. Deviation",
       title = "B. Std. Deviation of Correlation Between Variables Across Countries")

p <- ggarrange(p_cor_mean + 
                 theme(plot.margin=unit(c(0,-4,0,0), "cm")), 
               p_cor_sd + 
                 theme(plot.margin=unit(c(0,0,0,-4), "cm")),
               nrow = 1,
               widths = c(0.7, 0.3))

ggsave(p, filename = file.path(figures_global_dir, "cor_between_vars.png"),
       height = 9, width = 17.425)

