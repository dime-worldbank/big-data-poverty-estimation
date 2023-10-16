# LSMS Results

# Load data --------------------------------------------------------------------
pred_df <- file.path(data_dir, "DHS", "FinalData", "pov_estimation_results", 
                     "prediction") %>%
  list.files(pattern = "*.Rds",
             full.names = T) %>%
  str_subset("lsms") %>%
  str_subset("xgboost") %>%
  map_df(readRDS) 

# Make accuracy dataset --------------------------------------------------------
acc_df <- pred_df %>%
  group_by(country_code, country_name, estimation_type, feature_type, target_var_dep) %>%
  dplyr::summarise(r2 = cor(target_var, prediction)^2,
                   R2 = R2(pred = prediction, obs = target_var, form = "traditional"),
                   n = n()) %>%
  ungroup() %>%
  dplyr::mutate(feature_type_clean = case_when(
    feature_type == "all" ~ "All Features",
    feature_type == "all_changes" ~ "All Features",
    feature_type == "all_lsms" ~ "All Features",
    feature_type == "cnn_s2_bu" ~ "CNN: Built-Up Index",
    feature_type == "cnn_s2_ndvi" ~ "CNN: NDVI",
    feature_type == "cnn_s2_rgb" ~ "CNN: RGB",
    
    feature_type == "cnn_viirs_landsat" ~ "CNN: All Variables",
    feature_type == "cnn_viirs_landsat_bu" ~ "CNN: Built-Up Index",
    feature_type == "cnn_viirs_landsat_ndvi" ~ "CNN: NDVI",
    feature_type == "cnn_viirs_landsat_rgb" ~ "CNN: RGB",
    
    feature_type == "cnn_ntlharmon_landsat" ~ "CNN: All Variables",
    feature_type == "cnn_ntlharmon_landsat_bu" ~ "CNN: Built-Up Index",
    feature_type == "cnn_ntlharmon_landsat_ndvi" ~ "CNN: NDVI",
    feature_type == "cnn_ntlharmon_landsat_rgb" ~ "CNN: RGB",
    
    feature_type == "cnn_viirs_s2" ~ "CNN: All Variables",
    feature_type == "cnn_viirs_s2_bu" ~ "CNN: Built-Up Index",
    feature_type == "cnn_viirs_s2_ndvi" ~ "CNN: NDVI",
    feature_type == "cnn_viirs_s2_rgb" ~ "CNN: RGB",
    
    feature_type == "fb_prop" ~ "Facebook: Proportion",
    feature_type == "fb" ~ "Facebook",
    feature_type == "satellites" ~ "Day/Night Satellites",
    feature_type == "viirs" ~ "Nighttime Lights",
    feature_type == "ntlharmon" ~ "Nighttime Lights",
    feature_type == "landcover" ~ "Landcover",
    feature_type == "weatherclimate" ~ "Weather/Climate",
    feature_type == "weather" ~ "Weather",
    feature_type == "gc" ~ "Landcover - GlobCover",
    feature_type == "l8" ~ "Daytime Imagery",
    feature_type == "l7" ~ "Daytime Imagery",
    feature_type == "osm" ~ "OpenStreetMap",
    feature_type == "pollution" ~ "Pollution",
    feature_type == "pollution_aod" ~ "Pollution",
    feature_type == "satellites_changes" ~ "Day/Night Satellites",
    feature_type == "s1_sar" ~ "SAR",
    feature_type == "mosaik" ~ "MOSAIKS",
    TRUE ~ feature_type
  ))

# Figure -----------------------------------------------------------------------
p_pca <- acc_df %>%
  filter(estimation_type %in% "within_country_cv",
         acc_df$target_var_dep == "pca_allvars_mr") %>%
  ggplot(aes(x = reorder(feature_type_clean, r2, FUN = median, .desc =TRUE),
             y = r2)) +
  geom_point(aes(fill = country_name),
             pch = 21) +
  stat_summary(fun = median, geom = "text", col = "black",     
               vjust = -0.4, aes(label = paste(round(..y.., digits = 2)))) +
  labs(x = NULL,
       y = expression(r^2),
       fill = "Country",
       title = "A. Predicting Wealth Index") +
  theme_classic() +
  theme(axis.text.y = element_text(face = "bold"),
        plot.title = element_text(face = "bold"),
        plot.title.position = "plot") +
  coord_flip() 

p_cons <- acc_df %>%
  filter(estimation_type %in% "within_country_cv",
         target_var_dep == "poverty_measure") %>%
  ggplot(aes(x = reorder(feature_type_clean, r2, FUN = median, .desc =TRUE),
             y = r2)) +
  geom_point(aes(fill = country_name),
             pch = 21) +
  stat_summary(fun = median, geom = "text", col = "black",     
               vjust = -0.4, aes(label = paste(round(..y.., digits = 2)))) +
  labs(x = NULL,
       y = expression(r^2),
       fill = "Country",
       title = "B. Predicting Consumption") +
  theme_classic() +
  theme(axis.text.y = element_text(face = "bold"),
        plot.title = element_text(face = "bold"),
        plot.title.position = "plot") +
  coord_flip() 

p <- ggarrange(
  p_pca,
  p_cons,
  common.legend = T,
  legend = "right"
)

ggsave(p, 
       filename = file.path(figures_global_dir, "lsms_feature_type.png"),
       height = 6,
       width = 8)

