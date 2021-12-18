# Correlations of Features with Poverty

# Within country; show distribution. (color by continent?)

# Load data --------------------------------------------------------------------
df <- readRDS(file.path(data_dir, SURVEY_NAME, "FinalData", "Merged Datasets", "survey_alldata_clean.Rds"))

# Compute correlation ----------------------------------------------------------
# Compute correlation for each variable & country
df_long <- df %>%
  dplyr::select(country_name, 
                pca_allvars,
                starts_with("gc_"),
                starts_with("osm_"),
                starts_with("l8_"),
                starts_with("cnn_"),
                starts_with("fb_prop")) %>%
  pivot_longer(cols = -c(country_name, pca_allvars)) %>%
  dplyr::rename(variable = name) %>%
  dplyr::mutate(variable_cat = case_when(
    variable %>% str_detect("gc_") ~ "gc",
    variable %>% str_detect("osm_") ~ "osm",
    variable %>% str_detect("l8_") ~ "l8",
    variable %>% str_detect("cnn_s2_rgb_") ~ "cnn_s2_rgb",
    variable %>% str_detect("cnn_s2_ndvi_") ~ "cnn_s2_ndvi",
    variable %>% str_detect("cnn_s2_bu_") ~ "cnn_s2_bu",
    variable %>% str_detect("fb_prop") ~ "fb_prop",
    TRUE ~ variable
  ))

cor_df <- df_long %>%
  dplyr::filter(!is.na(pca_allvars),
                !is.na(value)) %>%
  group_by(variable, variable_cat, country_name) %>%
  dplyr::summarise(cor = cor(pca_allvars, value)) %>%
  ungroup()

# Figure -----------------------------------------------------------------------
cor_df %>%
  dplyr::filter(variable_cat %in% "fb_prop") %>%
  ggplot() +
  geom_boxplot(aes(x = cor,
                   y = reorder(variable, cor, FUN = mean, .desc =TRUE)))


