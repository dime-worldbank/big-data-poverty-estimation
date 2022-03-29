# Average Feature Importance

# Load Data --------------------------------------------------------------------
fi_df <- file.path(data_dir, SURVEY_NAME, "FinalData", "pov_estimation_results",
                   "feature_importance") %>% 
  list.files(pattern = "fi_global", # fi_within_country
             full.names = T) %>%
  str_subset("all.Rds") %>%
  map_df(readRDS) %>%
  as.data.frame()

# Clean Data -------------------------------------------------------------------
fi_df <- fi_df %>%
  dplyr::filter(target_var == "pca_allvars")

fi_df <- fi_df %>%
  dplyr::mutate(category = case_when(
    Feature %>% str_detect("osm_") ~ "OSM",
    Feature %>% str_detect("viirs_") ~ "NTL",
    Feature %>% str_detect("gc_") ~ "Land Cover",
    Feature %>% str_detect("worldclim_") ~ "Weather/Climate",
    Feature %>% str_detect("fb_") ~ "Facebook",
    Feature %>% str_detect("cnn_l8") ~ "CNN",
    Feature %>% str_detect("l8") ~ "DTL",
    Feature %>% str_detect("weather_") ~ "Weather/Climate",
    Feature %>% str_detect("pollution_") ~ "Pollution",
    Feature %>% str_detect("elevslope_") ~ "Elevation/Slope",
    TRUE ~ Feature
  ))

gain_med_df <- fi_df %>%
  group_by(Feature, category) %>%
  dplyr::summarise(Gain = median(Gain)) %>%
  arrange(desc(Gain)) %>%
  ungroup() %>%
  group_by(category) %>%
  dplyr::mutate(cat_id = 1:n())

gain_med_df_sub <- gain_med_df %>%
  dplyr::filter(cat_id <= 5)

fi_df %>%
  dplyr::filter(Feature %in% gain_med_df_sub$Feature) %>%
  ggplot() +
  geom_boxplot(aes(y = fct_reorder(Feature, Gain, .fun = median),
                   x = Gain,
                   fill = category)) +
  facet_wrap(~target_var)





