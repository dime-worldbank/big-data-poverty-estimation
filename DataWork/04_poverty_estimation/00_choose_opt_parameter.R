# Choose Optimal Parameters

# Load data --------------------------------------------------------------------
X <- df %>%
  dplyr::select_at(vars(starts_with("viirs_"),
                        starts_with("s1_sar_"),
                        starts_with("cnn_viirs_s2_"),
                        starts_with("fb_prop_"),
                        starts_with("fb_wp_prop"),
                        starts_with("osm_"),
                        starts_with("gc_"),
                        starts_with("l7_"),
                        starts_with("elevslope_"),
                        starts_with("weather_"),
                        starts_with("worldclim_"),
                        starts_with("pollution_"),
                        starts_with("mosaik_"))) 

#### Load data
df <- readRDS(file.path(data_dir, "DHS", "FinalData", "Merged Datasets", 
                        "survey_alldata_clean.Rds"))

df <- df %>%
  dplyr::filter(most_recent_survey %in% T)


df <- readRDS(file.path(data_dir, SURVEY_NAME, "FinalData", "pov_estimation_results",
                  "accuracy_appended.Rds"))
df <- df %>%
  dplyr::filter(estimation_type %in% "global_country_pred",
                feature_type == "all")

df %>%
  group_by(xg_param_set) %>%
  dplyr::summarise(r2 = mean(r2),
                   n = n())





