# Predict Poverty from Model to Dataset

# Load data --------------------------------------------------------------------
df <- readRDS(file.path(data_dir, "LAGOS_POINTS", "FinalData", "Merged Datasets", 
                        "survey_alldata_clean.Rds"))

X <- df %>%
  dplyr::select_at(vars(starts_with("viirs_"),
                        starts_with("s1_sar_"),
                        starts_with("fb_prop_"),
                        starts_with("fb_wp_prop"),
                        #starts_with("osm_"),
                        starts_with("gc_"),
                        starts_with("l7_"),
                        starts_with("elevslope_"),
                        starts_with("weather_"),
                        starts_with("worldclim_"),
                        starts_with("pollution_"))) %>%
  as.matrix()

m_files <- file.path(data_dir, "DHS", "FinalData", "pov_estimation_results", "models") %>%
  list.files()

df <- df %>%
  dplyr::select(uid)

for(i in 1:length(m_files)){
  xgb_model <- readRDS(file.path(data_dir, "DHS", "FinalData", "pov_estimation_results", "models", 
                                 m_files[i]))
  
  df[[paste0("wealth_pred_", i)]] <- predict(xgb_model, X)
}

write.csv(df, "~/Documents/Github/danfo/cleaning/poverty_estimates/data/data_extracted_to_base_layer/bri_30min_walking/pov_ests.csv", row.names = F)

