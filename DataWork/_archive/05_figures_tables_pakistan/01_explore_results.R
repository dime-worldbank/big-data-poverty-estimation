# Main Results

# To look into
# 1. Locations
# 2. Correlation along truth (below/above median pov)

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
#survey_sub_df <- survey_df %>%
#  dplyr::select(uid, latitude, longitude, continent_adj, urban_rural,
#                country_name, 
#                viirs_avg_rad, fb_prop_estimate_mau_upper_bound_30)

pred_df <- pred_df %>%
  dplyr::left_join(survey_df, by = "uid")

pred_df <- pred_df %>%
  dplyr::mutate(urban_rural = case_when(
    urban_rural == "U" ~ "Urban",
    urban_rural == "R" ~ "Rural"
  ))

# Add Pakistan ADM -------------------------------------------------------------
pak_adm1 <- readRDS(file.path(data_dir, "GADM", "RawData", "gadm36_PAK_1_sp.rds")) 
pak_adm2 <- readRDS(file.path(data_dir, "GADM", "RawData", "gadm36_PAK_2_sp.rds")) 
pak_adm3 <- readRDS(file.path(data_dir, "GADM", "RawData", "gadm36_PAK_3_sp.rds")) 

pak_adm1$area <- pak_adm1 %>% st_as_sf() %>% st_area() %>% as.numeric()
pak_adm2$area <- pak_adm2 %>% st_as_sf() %>% st_area() %>% as.numeric()
pak_adm3$area <- pak_adm3 %>% st_as_sf() %>% st_area() %>% as.numeric()

pred_sp <- pred_df
coordinates(pred_sp) <- ~longitude+latitude
crs(pred_sp) <- CRS("+init=epsg:4326")

pred_OVER_gadm <- over(pred_sp, pak_adm3)
pred_df$NAME_3 <- pred_OVER_gadm$NAME_3
pred_df$NAME_2 <- pred_OVER_gadm$NAME_2
pred_df$NAME_1 <- pred_OVER_gadm$NAME_1
pred_df$adm_area <- pred_OVER_gadm$area

# Prep Prediction Data ---------------------------------------------------------
#### Prep data
pred_sub_df <- pred_df %>%
  dplyr::filter(feature_type == "all",
                target_var == "wealth_index_score",
                estimation_type == "within_country_cv")

#### Aggregate to ADM
pred_adm1_df <- pred_sub_df %>%
  group_by(NAME_1) %>%
  summarise(across(where(is.numeric), list(mean = mean, sd = sd)),
            cor = cor(truth, prediction),
            adm_area = sum(adm_area),
            N = n()) %>%
  ungroup() %>%
  dplyr::mutate(adm = "ADM 1") %>%
  dplyr::mutate(r2 = cor^2)

pred_adm2_df <- pred_sub_df %>%
  group_by(NAME_2) %>%
  summarise(across(where(is.numeric), list(mean = mean, sd = sd)),
            cor = cor(truth, prediction),
            adm_area = sum(adm_area),
            N = n()) %>%
  ungroup() %>%
  dplyr::mutate(adm = "ADM 2") %>%
  dplyr::mutate(r2 = cor^2)

pred_adm3_df <- pred_sub_df %>%
  group_by(NAME_3) %>%
  summarise(across(where(is.numeric), list(mean = mean, sd = sd)),
            cor = cor(truth, prediction),
            adm_area = sum(adm_area),
            N = n()) %>%
  ungroup() %>%
  dplyr::mutate(adm = "ADM 3") %>%
  dplyr::mutate(r2 = cor^2) %>%
  dplyr::filter(!is.na(r2))

#### Correlation between within unit r2 and value
vars_to_use <- c("viirs_",
                 "gc_",
                 "osm_",
                 "l8_",
                 "cnn_",
                 "fb_prop_",
                 "worldclim_",
                 "worldpop_",
                 "elevslope_",
                 "globalmod_",
                 "pollution_aod_",
                 "pollution_s5p_",
                 "weather_") %>%
  paste(collapse = "|")

pred_adm1_cor_df <- pred_adm1_df %>%
  dplyr::select(NAME_1,
                r2,
                matches(vars_to_use)) %>%
  pivot_longer(cols = -c(NAME_1, r2)) %>%
  group_by(name) %>%
  dplyr::summarise(cor_modelr2_value = cor(r2, value)) %>%
  ungroup() %>%
  dplyr::mutate(variable = name %>% 
                  str_replace_all("_mean", "") %>% 
                  str_replace_all("_sd", "")) %>%
  clean_varnames()

pred_adm2_cor_df <- pred_adm2_df %>%
  dplyr::select(NAME_2,
                r2,
                matches(vars_to_use)) %>%
  pivot_longer(cols = -c(NAME_2, r2)) %>%
  group_by(name) %>%
  dplyr::summarise(cor_modelr2_value = cor(r2, value)) %>%
  ungroup() %>%
  dplyr::mutate(variable = name %>% 
                  str_replace_all("_mean", "") %>% 
                  str_replace_all("_sd", "")) %>%
  clean_varnames() 

pred_adm3_cor_df <- pred_adm3_df %>%
  dplyr::select(NAME_3,
                r2,
                matches(vars_to_use)) %>%
  pivot_longer(cols = -c(NAME_3, r2)) %>%
  group_by(name) %>%
  dplyr::summarise(cor_modelr2_value = cor(r2, value)) %>%
  ungroup() %>%
  dplyr::mutate(variable = name %>% 
                  str_replace_all("_mean", "") %>% 
                  str_replace_all("_sd", "")) %>%
  clean_varnames() 

pred_adm_all_df <- bind_rows(
  pred_adm1_df,
  pred_adm2_df,
  pred_adm3_df
)

# Correlations -----------------------------------------------------------------
cor_df <- survey_df %>%
  dplyr::select(uid,
                country_name, 
                continent_adj,
                gadm_uid,
                pca_allvars,
                wealth_index_score,
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
  pivot_longer(cols = -c(uid, gadm_uid, country_name, continent_adj,
                         pca_allvars, wealth_index_score)) %>%
  dplyr::rename(variable = name) %>%
  clean_varnames() %>%
  group_by(variable, variable_cat) %>%
  dplyr::summarise(cor = cor(wealth_index_score, value)) %>%
  ungroup() %>%
  dplyr::mutate(cor_abs = abs(cor)) %>%
  dplyr::filter(!is.na(cor))

#### Correlation Main Figure
p_cor_all <- cor_df %>%
  ggplot() +
  geom_vline(xintercept = 0, alpha = 0.4) +
  geom_boxplot(aes(x = cor,
                   y = reorder(variable_cat, cor, FUN = median, .desc =TRUE)),
               fill = "gray80",
               center = T,
               errorbar.draw = F) +
  labs(x = NULL,
       y = NULL,
       title = "A. Distribution of correlations across datasets") +
  theme_minimal() +
  theme(axis.text.y = element_text(color = "black"),
        plot.title.position = "plot",
        panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold")) +
  scale_x_continuous(limits = c(-0.8, 0.8),
                     breaks = seq(-0.8, 0.8, 0.2))

#### Facebook: Correlation Table
cor_fb_df <- cor_df %>%
  dplyr::filter(variable_cat %in% "Facebook Marketing") %>%
  clean_varnames() %>%
  arrange(-cor)

cor_var_df <- cor_df %>%
  clean_varnames() %>%
  dplyr::mutate(text_move = case_when(
    cor >= 0 ~ 0.1,
    cor < 0 ~ -0.1
  ))

## Facebook
p_fb <- cor_var_df %>%
  dplyr::filter(variable_cat %in% "Facebook Marketing") %>%
  dplyr::mutate(variable_clean_short = variable_clean_short %>%
                  str_replace_all("Samsung Galaxy phone",
                                  "Samsung Galaxy")) %>%
  dplyr::mutate(fb_var = variable_clean_short %>%
                  str_replace_all(".*:", "") %>%
                  str_squish(),
                fb_var_cat = variable_clean_short %>%
                  str_replace_all(":.*", "") %>%
                  str_squish()) %>%
  clean_varnames() %>%
  ggplot(aes(y = reorder(fb_var, cor, FUN = median, .desc =TRUE),
             xmin = 0,
             xmax = cor,
             x = cor,
             color = fb_var_cat,
             label = round(cor, 2))) +
  geom_point() +
  geom_linerange() +
  geom_text(aes(x = cor + text_move)) +
  labs(x = NULL,
       y = NULL,
       color = "Facebook\nVariable\nCategory",
       title = "B. Correlation of Facebook variables\nto wealth score") +
  theme_minimal() +
  theme(axis.text.y = element_text(color = "black"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold"),
        legend.position = "bottom",
        legend.margin=margin(0, 0, 0, -160),
        plot.title.position = "plot") +
  scale_color_brewer(palette = "Dark2", direction = 1) +
  guides(color=guide_legend(nrow=3,byrow=T)) +
  scale_x_continuous(limits = c(-0.2, 0.7))

## Pollution
# TODO: Top by pollutant (one for NO2, one for SO2, etc)
p_pollution <- cor_var_df %>%
  dplyr::filter(variable %>% str_detect("pollution")) %>%
  ggplot(aes(y = reorder(variable_clean, cor, FUN = median, .desc =TRUE),
             xmin = 0,
             xmax = cor,
             x = cor,
             label = round(cor, 2))) +
  geom_point() +
  geom_linerange() +
  geom_text(aes(x = cor + text_move)) +
  labs(x = NULL,
       y = NULL,
       title = "D. Correlation of pollution variables\nto wealth score") +
  theme_minimal() +
  theme(axis.text.y = element_text(color = "black"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold"),
        plot.title.position = "plot") +
  scale_x_continuous(limits = c(-0.15, 0.75))

## OSM
cor_var_osm_df <- cor_var_df %>%
  dplyr::filter(variable_cat %in% "OpenStreetMap")

cor_df_max <- cor_var_osm_df %>%
  group_by(variable_cat) %>%
  slice_max(order_by = cor, n = 10)

cor_df_min <- cor_var_osm_df %>%
  group_by(variable_cat) %>%
  slice_min(order_by = cor, n = 10)

cor_df_minmax <- bind_rows(
  cor_df_max,
  cor_df_min
) %>%
  distinct() 

p_osm <- cor_df_minmax %>%
  dplyr::mutate(osm_var = variable_clean %>%
                  str_replace_all("Distance: ", "") %>%
                  str_replace_all("Osm Length ", "") %>%
                  str_replace_all("5000m", ""),
                osm_var = case_when(
                  variable %>% str_detect("osm_length") ~ paste0("Road Length: ", osm_var),
                  variable %>% str_detect("osm_distmeters_poi") ~ paste0("POI Distance: ", osm_var),
                  variable %>% str_detect("osm_distmeters_road") ~ paste0("Road Distance: ", osm_var)
                )) %>%
  ggplot(aes(y = reorder(osm_var, cor, FUN = median, .desc =TRUE),
             xmin = 0,
             xmax = cor,
             x = cor,
             label = round(cor, 2))) +
  geom_point() +
  geom_linerange() +
  geom_text(aes(x = cor + (text_move*1.5))) +
  labs(x = NULL,
       y = NULL,
       title = "C. Correlation of select OSM variables\nto wealth score") +
  theme_minimal() +
  theme(axis.text.y = element_text(color = "black"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold"),
        plot.title.position = "plot") +
  scale_x_continuous(limits = c(-0.9, 0.75))

#### Arrange Figure
p_osm_pollution <- ggarrange(p_osm,
                             p_pollution,
                             ncol = 1,
                             heights = c(0.65, 0.35))

p_osm_pollution_fb <- ggarrange(p_fb, 
                                p_osm_pollution, 
                                nrow = 1)

p_cor <- ggarrange(p_cor_all,
                   p_osm_pollution_fb,
                   ncol = 1,
                   heights = c(0.35, 0.65))

ggsave(p_cor, filename = file.path(figures_pak_dir, "correlations.png"),
       height = 11, width = 9)



sink(file.path(tables_pak_dir, "fb_cor.tex"))

cat("\\begin{tabular}{rc} \n")
cat("\\hline \n")
cat("Variable & Correlation \\\\ \n")
cat("\\hline \n")

for(i in 1:nrow(cor_fb_df)){
  cat(cor_fb_df$variable_clean[i])
  cat(" & ")
  cat(cor_fb_df$cor[i] %>% round(2))
  cat(" \\\\ \n")
}

cat("\\hline \n")
cat("\\end{tabular}")

sink()



cor_df_max <- cor_df %>%
  group_by(variable_cat) %>%
  slice_max(order_by = cor, n = 1)

cor_df_min <- cor_df %>%
  group_by(variable_cat) %>%
  slice_min(order_by = cor, n = 1)

cor_df_minmax <- bind_rows(
  cor_df_max,
  cor_df_min
) %>%
  distinct() %>%
  clean_varnames()

cor_df_minmax %>%
  ggplot() +
  geom_col(aes(y = reorder(variable_clean, cor, FUN = median, .desc =TRUE),
               x = cor,
               fill = variable_cat))


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
  dplyr::mutate(r2 = cor(truth_mean, prediction_mean)^2) %>%
  ungroup() %>%
  dplyr::mutate(adm = adm %>% as.factor() %>% fct_rev()) %>%
  ggplot(aes(x = truth_mean,
             y = prediction_mean,
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
### Map & Scatterplot Parameters
PLOT_TITLE_SIZE <- 12

#### Maps
pak_adm1_s <- gSimplify(pak_adm1, tol = 0.05)
pak_adm1_s$id <- 1:length(pak_adm1_s)
pak_adm1_s@data <- pak_adm1@data

pak_adm1_sf <- merge(pak_adm1_s, pred_adm1_df, by = "NAME_1") %>% st_as_sf()
pak_adm1_sf <- merge(pak_adm1, pred_adm1_df, by = "NAME_1") %>% st_as_sf()

p_within_r2_map <- ggplot() +
  geom_sf(data = pak_adm1_sf,
          aes(fill = r2),
          color = "black") +
  theme_void() +
  labs(fill = expression("Within\nDistrict"~r^2),
       title = expression(bold("A. Within District"~r^2)),
       subtitle = "\n") +
  scale_fill_distiller(palette = "Spectral",
                       direction = 0) + 
  theme(legend.position = c(0.85,0.25),
        plot.title = element_text(hjust = 0, size = PLOT_TITLE_SIZE),
        plot.subtitle = element_text(size = PLOT_TITLE_SIZE))

#### Explain variation
## TODO: Run simple OLS on a number of variables, report
# (1) Coef, (2) Sign, (3) R2 [Coef plot]
# --- Could check with ALL variables, and just see what works.
if(F){
  View(pred_adm1_cor_df)
  
  pred_adm1_df %>%
    dplyr::filter(N >= 10) %>%
    ggplot() +
    geom_point(aes(x = r2,
                   y = viirs_avg_rad_sd))
}

p_explainvar_scatter <- pred_adm1_df %>%
  ## To Long
  dplyr::select(r2,
                l8_NDBI_sd,
                fb_prop_estimate_mau_upper_bound_21_sd,
                #osm_distmeters_poi_attraction_sd,
                osm_distmeters_road_tertiary_sd,
                viirs_avg_rad_sd) %>%
  pivot_longer(cols = -r2) %>%
  dplyr::mutate(variable = name %>%
                  str_replace_all("_sd|mean", "")) %>%
  clean_varnames() %>%
  dplyr::mutate(variable_clean = case_when(
    variable_clean == "Other device types: Oppo/VIVO/Cherry devices" ~ "[Facebook] Use Oppo/\nVIVO/Cherry Devices",
    variable == "l8_NDBI" ~ "[Landsat] Normalized Diff.\nBuilt-Up Index",
    variable == "osm_distmeters_poi_attraction" ~ "[OSM] Distance to Attractions",
    variable == "osm_distmeters_road_tertiary" ~ "[OSM] Distance to\nTertiary Roads",
    TRUE ~ variable_clean
  )) %>%
  
  ## Compute correlation
  group_by(name) %>%
  dplyr::mutate(cor = cor(r2, value),
                value_high = max(value)*0.93) %>%
  ungroup() %>%
  
  ## Figure
  ggplot(aes(x = r2,
             y = value)) +
  geom_smooth(method = lm, se = F,
              color = "gray50",
              size = 0.75) +
  geom_point(aes(fill = r2),
             pch = 21,
             color = "black",
             size = 3) +
  geom_richtext(aes(label = paste0("Cor = ", round(cor,2)),
                    x = 0.12,
                    y = value_high),
                size = 2.75) +
  facet_wrap(~variable_clean,
             scales = "free_y") +
  labs(x = expression("Within District"~r^2),
       y = "Within District Std. Dev. of Value",
       title = expression(bold("B. Correlation of within district"~r^2~"with")),
       subtitle = "within district standard deviation of values") +
  scale_fill_distiller(palette = "Spectral",
                       direction = 0) + 
  theme_classic() +
  theme(strip.background = element_blank(),
        strip.text = element_text(face = "bold", hjust = 0),
        plot.title = element_text(hjust = 0, size = PLOT_TITLE_SIZE),
        plot.subtitle = element_text(hjust = 0, size = PLOT_TITLE_SIZE, face = "bold"),
        legend.position = "none")

p <- ggarrange(p_within_r2_map,
               p_explainvar_scatter)

ggsave(p, filename = file.path(figures_pak_dir, "within_adm_r2.png"),
       height = 4.5, width = 8)

#### Correlations of Top Variables
top_cor <- pred_adm1_cor_df %>%
  arrange(-cor_modelr2_value) %>%
  head(15)

bottom_cor <- pred_adm1_cor_df %>%
  arrange(cor_modelr2_value) %>%
  head(15) %>%
  arrange(-cor_modelr2_value)

cor_topbottom <- bind_rows(top_cor,
                           bottom_cor)


head(pred_adm1_cor_df)



#### OTHER **********

#### Scatterplots
pred_sub_df %>%
  group_by(NAME_1) %>%
  dplyr::mutate(r2 = cor(truth, prediction)^2) %>%
  ungroup() %>%
  ggplot(aes(x = truth,
             y = prediction)) +
  geom_point(aes(color = urban_rural)) +
  geom_smooth(method = lm, se = F) +
  geom_richtext(aes(label = paste0("r<sup>2</sup> = ", round(r2,2)),
                    x = -100000,
                    y = 80000),
                size = 4.5) +
  facet_wrap(~NAME_1) +
  scale_color_manual(values = c("chartreuse4",
                                "chocolate2")) + 
  theme_classic() +
  theme(strip.text = element_text(face = "bold"))

