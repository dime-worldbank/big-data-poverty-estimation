# Feature Importance

## Grab best parameters
acc_df <- readRDS(file.path(data_dir, SURVEY_NAME, "FinalData", "pov_estimation_results",
                            "accuracy_appended_bestparam.Rds"))

xg_param_set_use <- acc_df %>%
  dplyr::filter(estimation_type == "within_country_cv",
                feature_type == "all",
                ml_model_type == "xgboost",
                target_var == "pca_allvars_mr",
                level_change == "changes") %>%
  
  dplyr::mutate(xg_eta = xg_eta %>% str_replace_all("[:punct:]", ""),
                xg_subsample = xg_subsample %>% str_replace_all("[:punct:]", "")) %>%
  dplyr::mutate(xg_param_set = paste(xg_max.depth,
                                     xg_eta,
                                     xg_nthread,
                                     xg_nrounds,
                                     xg_subsample,
                                     xg_objective,
                                     xg_min_child_weight,
                                     sep = "_")) %>%
  
  group_by(xg_param_set) %>%  
  dplyr::mutate(xg_param_set_cor = mean(cor)) %>%
  ungroup() %>%
  arrange(-xg_param_set_cor) %>%
  
  pull(xg_param_set) %>%
  str_replace_all(":", "") %>%
  str_replace_all("error_", "error")

## Load feature importance
fi_df <- file.path(data_dir, "DHS", "FinalData", "pov_estimation_results", "feature_importance") %>%
  #list.files(pattern = "predictions_within_country_cv_", # predictions_global_country_pred_ ""
  #           full.names = T) %>%
  list.files(pattern = "*.Rds",
             full.names = T) %>%
  str_subset("_changes_") %>%
  str_subset("pca_allvars") %>%
  str_subset("within_country_cv") %>%
  str_subset("_all_") %>%
  str_subset("xgboost") %>%
  #str_subset(paste0("all_changes",xg_param_set_use)) %>% # (1) Using all features and (2) Best Parameter type
  map_df(readRDS) %>%
  as.data.frame() %>%
  dplyr::rename(variable = Feature) 

# Figure -----------------------------------------------------------------------
feature_df <- fi_df %>%
  group_by(variable) %>%
  summarise(gain_avg = mean(Gain)) %>%
  ungroup() %>%
  clean_varnames() %>%
  mutate(variable_cat = case_when(
    variable %>% str_detect("osm_") & 
      variable %>% str_detect("_poi_") ~ "OpenStreetMap: POIs",
    
    variable %>% str_detect("osm_") & 
      variable %>% str_detect("_road_") ~ "OpenStreetMap: Roads",
    
    variable %>% str_detect("osm_length_") ~ "OpenStreetMap: Roads",
    
    TRUE ~ variable_cat
  )) %>%
  arrange(-gain_avg) 

feature_df %>%
  head(30) %>%
  dplyr::mutate(variable_clean = variable_clean %>% str_replace_all("L7 ", "Landsat ")) %>%
  ggplot(aes(x = gain_avg,
             xmin = 0,
             xmax = gain_avg,
             y = reorder(variable_clean, -sort(gain_avg)),
             color = variable_cat)) +
  geom_point() +
  geom_linerange() +
  geom_text(aes(x = gain_avg + 0.005,
                label = round(gain_avg,3))) +
  labs(x = "Gain",
       y = NULL,
       color = "Feature\nCategory") +
  xlim(0, max(feature_df$gain_avg) + 0.01) +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text = element_text(color = "black"))

ggsave(filename = file.path(figures_global_dir, "fi_changes.png"),
       height = 5,
       width = 8)



