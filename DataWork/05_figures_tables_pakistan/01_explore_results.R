# Main Results

# Load data --------------------------------------------------------------------
#### Results
results_df <- readRDS(file.path(data_dir, SURVEY_NAME, "FinalData", "pov_estimation_results",
                                "accuracy_appended.Rds"))
results_df <- results_df %>%
  dplyr::filter(country_name %in% "Pakistan") %>%
  dplyr::mutate(r2 = cor^2)

#### Predictions
pred_df <- file.path(data_dir, "DHS", "FinalData", "pov_estimation_results", "predictions") %>%
  list.files(pattern = ".Rds", 
             full.names = T) %>%
  #str_subset("pca_allvars_all") %>%
  
  # Subset to: Pakistan
  str_subset("_PK_") %>%
  
  map_df(readRDS)

#### Data
survey_df <- readRDS(file.path(data_dir, SURVEY_NAME, "FinalData", "Merged Datasets", "survey_alldata_clean.Rds"))
survey_df <- survey_df %>%
  dplyr::filter(country_code %in% "PK")

# Merge with select survey variables -------------------------------------------
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

# Add Pakistan ADM -------------------------------------------------------------
pak_adm1 <- readRDS(file.path(data_dir, "GADM", "RawData", "gadm36_PAK_3_sp.rds")) 
pak_adm2 <- readRDS(file.path(data_dir, "GADM", "RawData", "gadm36_PAK_3_sp.rds")) 
pak_adm3 <- readRDS(file.path(data_dir, "GADM", "RawData", "gadm36_PAK_3_sp.rds")) 

pak_adm <- readRDS(file.path(data_dir, "GADM", "RawData", "gadm36_PAK_3_sp.rds")) 

pred_sp <- pred_df
coordinates(pred_sp) <- ~longitude+latitude
crs(pred_sp) <- CRS("+init=epsg:4326")

pred_OVER_gadm <- over(pred_sp, pak_adm)
pred_df$NAME_3 <- pred_OVER_gadm$NAME_3
pred_df$NAME_2 <- pred_OVER_gadm$NAME_2
pred_df$NAME_1 <- pred_OVER_gadm$NAME_1

# Correlations -----------------------------------------------------------------

# Predictions ------------------------------------------------------------------
#### Prep data
pred_sub_df <- pred_df %>%
  dplyr::filter(feature_type == "all",
                target_var == "wealth_index_score",
                estimation_type == "within_country_cv")

pred_adm1_df <- pred_sub_df %>%
  group_by(NAME_1) %>%
  dplyr::summarise(cor = cor(truth, prediction),
                   truth = mean(truth),
                   prediction = mean(prediction),
                   N = n()) %>%
  ungroup() %>%
  dplyr::mutate(adm = "ADM 1") %>%
  dplyr::mutate(r2 = cor^2)

pred_adm2_df <- pred_sub_df %>%
  group_by(NAME_2) %>%
  dplyr::summarise(cor = cor(truth, prediction),
                   truth = mean(truth),
                   prediction = mean(prediction),
                   N = n()) %>%
  ungroup() %>%
  dplyr::mutate(adm = "ADM 2") %>%
  dplyr::mutate(r2 = cor^2)

pred_adm3_df <- pred_sub_df %>%
  group_by(NAME_3) %>%
  dplyr::summarise(cor = cor(truth, prediction),
                   truth = mean(truth),
                   prediction = mean(prediction),
                   N = n()) %>%
  ungroup() %>%
  dplyr::mutate(adm = "ADM 3") %>%
  dplyr::mutate(r2 = cor^2)

pred_adm_all_df <- bind_rows(
  pred_adm1_df,
  pred_adm2_df,
  pred_adm3_df
)

#### Survey Cluster Prediction
pred_sub_df_u <- pred_sub_df[pred_sub_df$urban_rural %in% "Urban",]
pred_sub_df_r <- pred_sub_df[pred_sub_df$urban_rural %in% "Rural",]

r2_all   <- cor(pred_sub_df$truth, pred_sub_df$prediction)^2
r2_urban <- cor(pred_sub_df_u$truth, pred_sub_df_u$prediction)^2
r2_rural <- cor(pred_sub_df_r$truth, pred_sub_df_r$prediction)^2

# Target var: wealth_index_score
if(T){
  TEXT_X <- -190000
  TEXT_Y_TOP <- 170000
  TEXT_Y_INCR <- 21000
  FONT_SIZE <- 4.5
}

if(F){
  TEXT_X <- -190000
  TEXT_Y_TOP <- 140000
  TEXT_Y_INCR <- 15000
  FONT_SIZE <- 4.5
}

#### Survey Cluster Level
p_cluster <- ggplot() +
  geom_point(data = pred_sub_df,
             aes(x = truth,
                 y = prediction,
                 fill = urban_rural),
             pch = 21,
             color = "gray90",
             size = 2) +
  geom_richtext(aes(label = paste0("All r<sup>2</sup>: ", round(r2_all,2)),
                    x = TEXT_X,
                    y = TEXT_Y_TOP),
                color = "black",
                hjust = 0,
                fill = NA, 
                label.color = NA,
                size = FONT_SIZE) +
  geom_richtext(aes(label = paste0("Urban r<sup>2</sup>: ", round(r2_urban,2)),
                    x = TEXT_X,
                    y = TEXT_Y_TOP - TEXT_Y_INCR),
                color = "chocolate2",
                hjust = 0,
                fill = NA, 
                label.color = NA,
                size = FONT_SIZE) + 
  geom_richtext(aes(label = paste0("Rural r<sup>2</sup>: ", round(r2_rural,2)),
                    x = TEXT_X,
                    y = TEXT_Y_TOP - TEXT_Y_INCR*2),
                color = "chartreuse4",
                hjust = 0,
                fill = NA, 
                label.color = NA,
                size = FONT_SIZE) +
  theme_classic() +
  scale_fill_manual(values = c("chartreuse4", "chocolate2")) +
  labs(x = "True Wealth Score",
       y = "Estimated Wealth Score",
       title = "A. Survey cluster level") +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold"),
        axis.text = element_text(size = 7),
        axis.title = element_text(size = 10))

#### ADM Level
p_adm <- pred_adm_all_df %>%
  group_by(adm) %>%
  dplyr::mutate(r2 = cor(truth, prediction)^2) %>%
  ungroup() %>%
  dplyr::mutate(adm = adm %>% as.factor() %>% fct_rev()) %>%
  ggplot(aes(x = truth,
             y = prediction,
             size = N)) +
  geom_point(pch = 21,
             fill = "gray80",
             color = "black") +
  facet_wrap(~adm, nrow = 1) +
  geom_richtext(aes(label = paste0("r<sup>2</sup> = ", round(r2,2)),
                    x = -100000,
                    y = 80000),
                size = 4.5) +
  labs(x = "True Wealth Score",
       y = "Estimated Wealth Score",
       title = "B. Aggregating values to administrative divisions",
       size = "N Survey\nClusters\nin ADM") +
  theme_classic() +
  theme(legend.position = "right",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(face = "bold"),
        plot.title = element_text(face = "bold"),
        axis.text = element_text(size = 7),
        axis.title = element_text(size = 10))

#### Arrange/Export
p <- ggarrange(p_cluster, 
               p_adm,
          nrow = 1,
          widths = c(0.3, 0.7))

ggsave(p, filename = file.path(figures_pak_dir, "scatterplots.png"),
       height = 4, width = 11)

# Within ADM r2 ----------------------------------------------------------------
pak_adm2_sf <- merge(pak_adm2, pred_adm2_df, by = "NAME_2") %>% st_as_sf()
pak_adm1_sf <- merge(pak_adm1, pred_adm1_df, by = "NAME_1") %>% st_as_sf()

ggplot() +
  geom_sf(data = pak_adm2_sf,
          aes(fill = r2)) +
  theme_void()

ggplot() +
  geom_sf(data = pak_adm1_sf,
          aes(fill = r2)) +
  theme_void()

pred_adm1_df$cor %>% hist()

head(pred_adm1_df)









