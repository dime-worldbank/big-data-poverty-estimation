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
  dplyr::mutate(variable_clean = case_when(
    is.na(variable_clean) ~ variable,
    TRUE ~ variable_clean
  )) %>%
  dplyr::mutate(variable_clean = case_when(
    variable_clean %in% "viirs_avg_rad" ~ "[VIIRS] Nighttime Lights",
    variable_clean %in% "gc_190" ~ "[Globcover] Urban Land",
    variable_clean %in% "globalmod_mean" ~ "[Global Human Modification Index]",
    variable_clean %in% "osm_length_residential_5000m" ~ "[OSM] Residential Road Length",
    variable_clean %in% "cnn_s2_rgb_pc11" ~ "[Daytime Imagery, CNN: RGB] Feature 1",
    variable_clean %in% "cnn_s2_ndvi_pc7" ~ "[Daytime Imagery, CNN: NDVI] Feature 1",
    variable_clean %in% "cnn_s2_bu_pc16"  ~ "[Daytime Imagery, CNN: Built-Up Index] Feature 1",
    variable_clean %in% "worldpop_10km" ~ "[World Pop] Population",
    variable_clean %in% "pollution_s5p_tropospheric_NO2_column_number_density" ~ 
      "[Pollution, Sentinel-5P] Tropospheric NO2",
    variable_clean %in% "l8_B2" ~ "[Landsat 8] Blue Surface Reflectance (B2)",
    variable_clean %in% "Interests: Restaurants" ~ "[Facebook] Interests: Restaurants",
    variable_clean %in% "weather_q4_minimum_2m_air_temperature" ~ "[Weather] Quarter 4 Min. Temperature",
    variable_clean %in% "pollution_aod_047" ~ "[Pollution, MODIS] AOD",
    variable_clean %in% "worldclim_bio_6" ~ "[WorldClim] Bio 6",
    variable_clean %in% "elevslope_slope" ~ "[Elevation/Slope] Slope",
    TRUE ~ variable_clean
  ))

### Figure
p <- cor_topvars_clean_df %>%
  ggplot() +
  geom_vline(xintercept = 0, alpha = 0.5) +
  geom_boxplot(aes(x = cor,
                   y = reorder(variable_clean, cor, FUN = median, .desc =TRUE)),
               fill = "orange") +
  labs(title = "Correlation of Select Variables to Poverty Across Countries",
       subtitle = "The variable with the highest median correlation for each dataset is shown",
       y = NULL,
       x = "Correlation between variable and poverty") + 
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10),
                     limits = c(min(cor_topvars_clean_df$cor),
                                1)) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"),
        axis.text.y = element_text(face = "bold", color = "black"))

ggsave(p,
       filename = file.path(figures_global_dir, "cor_topvar_category.png"),
       height = 5,
       width = 8.5)

# CORRELATIONS BY DATASET ------------------------------------------------------

# ** Facebook ------------------------------------------------------------------
cor_df %>%
  dplyr::filter(variable_cat %in% "Facebook Marketing") %>%
  ggplot() +
  geom_boxplot(aes(x = cor,
                   y = reorder(variable_clean, cor, FUN = mean, .desc =TRUE)),
               fill = "orange") +
  labs(y = NULL,
       x = "Correlation")

# ** Globcover -----------------------------------------------------------------
p <- cor_df %>%
  dplyr::filter(variable_cat %in% "Land Cover") %>%
  ggplot() +
  geom_vline(xintercept = 0, alpha = 0.5) +
  geom_boxplot(aes(x = cor,
                   y = reorder(variable_clean, cor, FUN = mean, .desc =TRUE)),
               fill = "orange") +
  labs(y = NULL,
       x = "Correlation",
       title = "Correlation of Globcover Variables to Asset Index") +
  theme_minimal() +
  theme(axis.text.y = element_text(color = "black"),
        plot.title = element_text(face = "bold"))

ggsave(p,
       filename = file.path(figures_global_dir, "cor_globcover.png"),
       height = 6,
       width = 9.5)

# ** OpenStreetMap -------------------------------------------------------------
p <- cor_df %>%
  dplyr::filter(variable_cat %in% "OpenStreetMap") %>%
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
                  str_squish() %>%
                  tools::toTitleCase() %>%
                  str_replace_all("\\ball\\b", "All") %>%
                  str_replace_all("\\bany\\b", "Any")) %>%
  ggplot() +
  geom_vline(xintercept = 0, alpha = 0.5) +
  geom_boxplot(aes(x = cor,
                   y = reorder(variable_clean, cor, FUN = mean, .desc =TRUE),
                   fill = osm_cat)) +
  labs(y = NULL,
       x = "Correlation",
       fill = "Variable Type",
       title = "Correlation of OSM Variables to Asset Index") +
  theme_minimal() +
  theme(axis.text.y = element_text(color = "black"),
        plot.title = element_text(face = "bold"))

ggsave(p,
       filename = file.path(figures_global_dir, "cor_osm.png"),
       height = 10,
       width = 8.5)


