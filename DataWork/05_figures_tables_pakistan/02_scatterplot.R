# Scatterplot - Predicted vs True

# Load Data --------------------------------------------------------------------
pred_df <- file.path(data_dir, "DHS", "FinalData", "pov_estimation_results", "predictions") %>%
  #list.files(pattern = "predictions_within_country_cv_", # predictions_global_country_pred_ ""
  #           full.names = T) %>%
  
  list.files(pattern = ".Rds", 
             full.names = T) %>%
  
  # Subset to:
  # -- Prediction of pca_allvars variable
  # -- Prediction using all features
  #str_subset("pca_allvars_all.Rds") %>%
  str_subset("pca_allvars_all") %>%
  
  # Subset to: Pakistan
  #str_subset("_PK_") %>%
  
  map_df(readRDS)

# Merge with select survey variables -------------------------------------------
survey_df <- readRDS(file.path(data_dir, SURVEY_NAME, "FinalData", "Merged Datasets", "survey_alldata_clean.Rds"))

survey_df <- survey_df %>%
  dplyr::select(uid, latitude, longitude, continent_adj, urban_rural,
                country_name)

pred_df <- pred_df %>%
  dplyr::left_join(survey_df, by = "uid")

pred_df <- pred_df %>%
  dplyr::mutate(urban_rural = case_when(
    urban_rural == "U" ~ "Urban",
    urban_rural == "R" ~ "Rural"
  ))

# Cleanup/subset ---------------------------------------------------------------
pred_df <- pred_df %>%
  dplyr::filter(country_name %in% "Pakistan")

# Global Scatterplot -----------------------------------------------------------
pred_best_df <- pred_df %>%
  dplyr::filter(estimation_type %in% "global_country_pred")

cor_val <- cor(pred_best_df$truth, pred_best_df$prediction)

p_scatter_global <- pred_best_df %>%
  ggplot(aes(x = truth,
             y = prediction,
             color = urban_rural)) +
  geom_point(size = 0.5,
             alpha = 1) +
  scale_color_manual(values = c("chartreuse4", "chocolate1")) +
  labs(color = NULL,
       title = "Predicted vs. True Wealth Scores",
       subtitle = paste0("Correlation: ", round(cor_val, 2)),
       x = "True",
       y = "Predicted") +
  theme_minimal() +
  theme(legend.position = c(0.1, 0.9),
        legend.box.background = element_rect(colour = "black"),
        plot.title = element_text(face = "bold", size = 16)) +
  guides(color = guide_legend(override.aes = list(size = 2)),
         alpha = guide_legend(override.aes = list(alpha = 1)))

p_scatter_global
