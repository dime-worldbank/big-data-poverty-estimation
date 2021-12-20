# Global Scatterplot - Predicted vs True

# TODO:
# (1) Global correlation using globally best parameters
# (2) Individual country correlation using globally best parameters
# (3) Individual country correlation using country-specific best parmeters
# ------For this, can load all predictions, compute correlations, and take
# ------best across countries. In this figure (or table), report estimation type

# Load Data --------------------------------------------------------------------
pred_df <- file.path(data_dir, "DHS", "FinalData", "pov_estimation_results", "predictions") %>%
  list.files(pattern = "predictions_within_country_cv_", # predictions_global_country_pred_ ""
             full.names = T) %>%
  # Subset to:
  # -- Prediction of pca_allvars variable
  # -- Prediction using all features
  str_subset("pca_allvars_all.Rds") %>%
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

# Global Scatterplot -----------------------------------------------------------
cor_val <- cor(pred_df$truth, pred_df$prediction)

p_scatter_global <- pred_df %>%
  ggplot(aes(x = truth,
             y = prediction,
             color = urban_rural)) +
  geom_point(size = 0.25,
             alpha = 0.3) +
  scale_color_manual(values = c("chartreuse4", "chocolate1")) +
  labs(color = NULL,
       title = "\n\n\n\nPredicted vs. True Wealth Scores",
       subtitle = paste0("Correlation: ", round(cor_val, 2)),
       x = "True\n\n\n\n\n\n",
       y = "Predicted") +
  theme_minimal() +
  theme(legend.position = c(0.1, 0.9),
        legend.box.background = element_rect(colour = "black"),
        plot.title = element_text(face = "bold", size = 16)) +
  guides(color = guide_legend(override.aes = list(size = 2)),
         alpha = guide_legend(override.aes = list(alpha = 1)))

p_scatter_global

# Country Scatterplot ----------------------------------------------------------
p_scatter_country <- pred_df %>%
  group_by(country_name) %>%
  dplyr::mutate(cor_val = cor(truth, prediction)) %>%
  dplyr::mutate(country_name = paste0(country_name, "\nCorrelation: ", 
                                      round(cor_val, 2))) %>%
  ungroup() %>%
  dplyr::mutate(country_name = reorder(country_name, cor_val, FUN = median, .desc =T)) %>%
  ggplot(aes(x = truth,
             y = prediction,
             color = urban_rural)) +
  geom_point(size = 0.4, # 0.25
             alpha = 0.7) + # 0.3
  scale_color_manual(values = c("chartreuse4", "chocolate1")) +
  labs(color = NULL,
       title = "Predicted vs. True Wealth Scores",
       x = "True Poverty Level",
       y = "Predicted Poverty Level") +
  theme_minimal() +
  theme(legend.position = "top",
        legend.box.background = element_rect(colour = "black"),
        plot.title = element_text(face = "bold", size = 16),
        strip.text = element_text(face = "bold")) +
  guides(color = guide_legend(override.aes = list(size = 2)),
         alpha = guide_legend(override.aes = list(alpha = 1))) +
  facet_wrap(~country_name)

SCALE = 1.5
ggsave(p_scatter_country, 
       filename = "~/Desktop/test.png",
       height = 10*SCALE, 
       width = 8*SCALE)



#ggsave(p, filename = file.path(figures_dir, "allcountries_within_scatter.png"),
#       height = 10, width = 10)

# To Long (for map) ----------------------------------------------------------------------
# Stack truth and prediction
pred_long_df <- pred_df %>%
  dplyr::select(truth, prediction, latitude, longitude, uid) %>%
  pivot_longer(cols = -c(uid, latitude, longitude)) %>%
  dplyr::mutate(name = case_when(
    name == "truth" ~ "True Wealth Score",
    name == "prediction" ~ "Predicted Wealth Score"
  )) %>%
  arrange(desc(value))

# Map --------------------------------------------------------------------------
#### Basemap
shift <- 0.1
basemap <- get_stamenmap(bbox = c(left = min(survey_df$longitude) - shift,
                                  bottom = min(survey_df$latitude) - shift,
                                  right = max(survey_df$longitude) + shift,
                                  top = max(survey_df$latitude) + shift),
                         maptype = "toner-background", 
                         crop = FALSE,
                         zoom = 4)

#### Map
p_map <- ggmap(basemap) +
  geom_point(data = pred_long_df,
             aes(x = longitude,
                 y = latitude),
             size = 0.15,
             color = "black") + 
  geom_point(data = pred_long_df,
             aes(x = longitude,
                 y = latitude,
                 color = value),
             size = 0.075) + 
  scale_color_distiller(palette = "Spectral",
                        direction = 1) +
  theme_void() +
  theme(strip.text = element_text(face = "bold",
                                  size = 16)) +
  labs(color = "Wealth\nScore") +
  facet_wrap(~name,
             ncol = 1)

#ggsave(p, filename = file.path(figures_dir, "allcountries_within_map.png"),
#       height = 14, width = 20)

# Arrange/Export ---------------------------------------------------------------
p <- ggarrange(p_scatter,
               p_map,
               ncol = 2,
               widths = c(0.4, 0.6),
               heights = c(0.8, 1))

ggsave(p, filename = file.path(figures_dir, "global_scatter_map_within_country_cv.png"),
       height = 12, width = 18)



