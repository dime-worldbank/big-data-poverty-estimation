# Global Scatterplot - Predicted vs True

# Load Data --------------------------------------------------------------------
pred_df <- file.path(data_dir, "DHS", "FinalData", "pov_estimation_results", "predictions") %>%
  #list.files(pattern = "predictions_within_country_cv_", # predictions_global_country_pred_ ""
  #           full.names = T) %>%
  list.files(pattern = "*.Rds",
             full.names = T) %>%
  str_subset("pca_allvars_all.Rds") %>%
  map_df(readRDS)

# Select best estimation type for each country ---------------------------------
pred_df <- pred_df %>%
  mutate(country_est_id = paste(estimation_type, country_code))

pred_df <- pred_df[pred_df$estimation_type %in% "within_country_cv",]

cor_df <- pred_df %>%
  group_by(country_est_id, estimation_type, country_code) %>%
  dplyr::summarise(cor = cor(truth, prediction)) %>%
  
  group_by(country_code) %>% 
  slice_max(order_by = cor, n = 1) %>%
  
  mutate(r2 = cor^2)

pred_df <- pred_df[pred_df$country_est_id %in% cor_df$country_est_id,]

# Merge with select survey variables -------------------------------------------
survey_df <- readRDS(file.path(data_dir, SURVEY_NAME, "FinalData", "Merged Datasets", "survey_alldata_clean.Rds"))

survey_df <- survey_df %>%
  dplyr::select(uid, latitude, longitude, continent_adj, urban_rural)

pred_df <- pred_df %>%
  dplyr::left_join(survey_df, by = "uid")

pred_df <- pred_df %>%
  dplyr::mutate(urban_rural = case_when(
    urban_rural == "U" ~ "Urban",
    urban_rural == "R" ~ "Rural"
  ))

# Scatterplot ------------------------------------------------------------------

pred_df_u <- pred_df[pred_df$urban_rural %in% "Urban",]
pred_df_r <- pred_df[pred_df$urban_rural %in% "Rural",]

r2_all   <- cor(pred_df$truth,   pred_df$prediction)^2
r2_urban <- cor(pred_df_u$truth, pred_df_u$prediction)^2
r2_rural <- cor(pred_df_r$truth, pred_df_r$prediction)^2

TEXT_X <- -4

p_scatter <- ggplot() +
  geom_point(aes(color = urban_rural,
                 x = truth,
                 y = prediction),
             data = pred_df,
             size = 0.25,
             alpha = 0.3) +
  geom_richtext(aes(label = paste0("All r<sup>2</sup>: ", round(r2_all,2)),
                    x = TEXT_X,
                    y = 4),
                color = "black",
                hjust = 0,
                fill = NA, 
                label.color = NA) +
  geom_richtext(aes(label = paste0("Urban r<sup>2</sup>: ", round(r2_urban,2)),
                    x = TEXT_X,
                    y = 3.75),
                color = "chocolate2",
                hjust = 0,
                fill = NA, 
                label.color = NA) + 
  geom_richtext(aes(label = paste0("Rural r<sup>2</sup>: ", round(r2_rural,2)),
                    x = TEXT_X,
                    y = 3.5),
                color = "chartreuse4",
                hjust = 0,
                fill = NA, 
                label.color = NA) +
  # geom_smooth(aes(x = truth,
  #                 y = prediction),
  #             data = pred_df %>% dplyr::filter(urban_rural %in% "Urban"),
  #             method = "lm",
  #             se = FALSE,
  #             color = "darkorange3") +
  # geom_smooth(aes(x = truth,
  #                 y = prediction),
  #             data = pred_df %>% dplyr::filter(urban_rural %in% "Rural"),
  #             method = "lm",
  #             se = FALSE,
#             color = "darkgreen") +
scale_color_manual(values = c("chartreuse4", "chocolate2")) +
  labs(color = NULL,
       title = "\n\n\n\nPredicted vs. True Wealth Scores",
       x = "True Wealth Asset Index\n\n\n\n\n\n",
       y = "Predicted Wealth Asset Index") +
  theme_minimal() +
  theme(legend.position = c(0.9, 0.1),
        legend.box.background = element_rect(colour = "black"),
        plot.title = element_text(face = "bold", size = 16),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  guides(color = guide_legend(override.aes = list(size = 2)),
         alpha = guide_legend(override.aes = list(alpha = 1)))

# To Long ----------------------------------------------------------------------
# Stack truth and prediction
pred_long_df <- pred_df %>%
  dplyr::select(truth, prediction, latitude, longitude, uid, country_code) %>%
  pivot_longer(cols = -c(uid, latitude, longitude, country_code)) %>%
  dplyr::mutate(name = case_when(
    name == "truth" ~ "True Wealth Score",
    name == "prediction" ~ "Predicted Wealth Score"
  )) 

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



#ggsave(p, filename = file.path(figures_dir, "allcountries_within_scatter.png"),
#       height = 10, width = 10)

# Arrange/Export ---------------------------------------------------------------
p <- ggarrange(p_scatter,
               p_map,
               ncol = 2,
               widths = c(0.4, 0.6),
               heights = c(0.8, 1))

ggsave(p, filename = file.path(figures_global_dir, "global_scatter_map_within_country_cv.png"),
       height = 12, width = 18)




