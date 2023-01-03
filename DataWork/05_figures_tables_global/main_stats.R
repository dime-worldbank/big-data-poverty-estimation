# Main Paper Stats

# Load data --------------------------------------------------------------------
survey_chn_df <- readRDS(file.path(data_dir, SURVEY_NAME, "FinalData", "Merged Datasets", "survey_alldata_clean_changes_cluster_predictions.Rds"))
survey_df     <- readRDS(file.path(data_dir, SURVEY_NAME, "FinalData", "Merged Datasets", "survey_alldata_clean_predictions.Rds"))
acc_df        <- readRDS(file.path(data_dir, SURVEY_NAME, "FinalData", "pov_estimation_results",
                                   "accuracy_appended_bestparam.Rds"))

acc_levels_best_df <- acc_df %>%
  dplyr::filter(feature_type %in% "all",
                estimation_type %in% "best",
                target_var %in% "pca_allvars_mr",
                level_change %in% "levels")

acc_changes_best_df <- acc_df %>%
  dplyr::filter(feature_type %in% "all_changes",
                estimation_type %in% "best",
                target_var %in% "pca_allvars",
                level_change %in% "changes")

# Create datasets --------------------------------------------------------------
survey_chn_dist_df <- survey_chn_df %>%
  dplyr::filter(!is.na(predict_pca_allvars_best)) %>%
  group_by(country_code, gadm_uid) %>%
  dplyr::summarise(predict_pca_allvars_best = mean(predict_pca_allvars_best),
                   pca_allvars = mean(pca_allvars)) %>%
  group_by(country_code) %>%
  dplyr::summarise(r2 = cor(predict_pca_allvars_best, pca_allvars)^2)

# Stats: Levels Results --------------------------------------------------------
#### N Clusters
survey_df %>%
  dplyr::filter(!is.na(predict_pca_allvars_mr_best)) %>%
  nrow() %>%
  prettyNum(big.mark=",",scientific=FALSE) %>%
  write(file.path(stats_global_dir, "n_villages_levels.txt"))

#### N Countries
survey_df %>%
  dplyr::filter(!is.na(predict_pca_allvars_mr_best)) %>%
  pull(country_code) %>%
  unique() %>%
  length() %>%
  write(file.path(stats_global_dir, "n_countries_levels.txt"))

#### Income Groups
acc_levels_df$income %>% table()

#### Overall r2
survey_df %>%
  dplyr::filter(!is.na(predict_pca_allvars_mr_best)) %>%
  group_by(country_code) %>%
  dplyr::summarise(r2 = cor(predict_pca_allvars_mr_best, pca_allvars_mr)^2) %>%
  ungroup() %>%
  pull(r2) %>%
  mean() %>%
  round(2) %>%
  "*"(100) %>%
  paste0("\\%") %>%
  write(file.path(stats_global_dir, "levels_village_r2.txt")) 

survey_df %>%
  dplyr::filter(!is.na(predict_pca_allvars_mr_best)) %>%
  group_by(country_code, gadm_uid) %>%
  dplyr::summarise(predict_pca_allvars_mr_best = mean(predict_pca_allvars_mr_best),
                   pca_allvars_mr = mean(pca_allvars_mr)) %>%
  group_by(country_code) %>%
  dplyr::summarise(r2 = cor(predict_pca_allvars_mr_best, pca_allvars_mr)^2) %>%
  ungroup() %>%
  pull(r2) %>%
  mean() %>%
  round(2) %>%
  "*"(100) %>%
  paste0("\\%") %>%
  write(file.path(stats_global_dir, "levels_district_r2.txt")) 

#### Low / High Income r2
acc_levels_best_df %>%
  dplyr::filter(income %in% "Low income") %>%
  pull(r2) %>%
  mean() %>%
  round(2) %>%
  "*"(100) %>%
  paste0("\\%") %>%
  write(file.path(stats_global_dir, "levels_low_income_r2.txt"))

#### Upper middle income r2
acc_levels_best_df %>%
  dplyr::filter(income %in% "Upper middle income") %>%
  pull(r2) %>%
  mean() %>%
  round(2) %>%
  "*"(100) %>%
  paste0("\\%") %>%
  write(file.path(stats_global_dir, "levels_uppermiddle_income_r2.txt"))

#### N Features
survey_df %>%
  dplyr::select_at(vars(starts_with("viirs_"),
                        starts_with("cnn_viirs_s2_"),
                        starts_with("fb_prop_"),
                        starts_with("fb_wp_prop"),
                        starts_with("osm_"),
                        starts_with("gc_"),
                        starts_with("l7_"),
                        starts_with("elevslope_"),
                        starts_with("weather_"),
                        starts_with("worldclim_"),
                        starts_with("pollution_"))) %>%
  ncol() %>%
  write(file.path(stats_global_dir, "n_features_levels.txt"))

#### Model type that works best
est_type_best <- acc_df %>%
  dplyr::filter(feature_type == "all",
                estimation_type != "best") %>%
  group_by(country) %>%
  slice_max(order_by = r2, n = 1) %>%
  ungroup() %>%
  pull(estimation_type)

est_type_best %>% table()
(est_type_best %>% table()) / length(est_type_best)

# Stats: Results Changes ------------------------------------------------
#### N Obs Changes, Villages
survey_chn_df %>%
  dplyr::filter(!is.na(predict_pca_allvars_best)) %>%
  nrow() %>%
  write(file.path(stats_global_dir, "n_obs_changes_villages.txt"))

#### N Countries
survey_chn_df %>%
  dplyr::filter(!is.na(predict_pca_allvars_best)) %>%
  pull(country_code) %>%
  unique() %>%
  length() %>%
  write(file.path(stats_global_dir, "n_countries_changes.txt"))

#### Overall r2
survey_chn_df %>%
  dplyr::filter(!is.na(predict_pca_allvars_best)) %>%
  group_by(country_code) %>%
  dplyr::summarise(r2 = cor(predict_pca_allvars_best, pca_allvars)^2) %>%
  ungroup() %>%
  pull(r2) %>%
  mean() %>%
  round(2) %>%
  "*"(100) %>%
  paste0("\\%") %>%
  write(file.path(stats_global_dir, "changes_village_r2.txt")) 

survey_chn_df %>%
  dplyr::filter(!is.na(predict_pca_allvars_best)) %>%
  group_by(country_code, gadm_uid) %>%
  dplyr::summarise(predict_pca_allvars_best = mean(predict_pca_allvars_best),
                   pca_allvars = mean(pca_allvars)) %>%
  group_by(country_code) %>%
  dplyr::summarise(r2 = cor(predict_pca_allvars_best, pca_allvars)^2) %>%
  ungroup() %>%
  pull(r2) %>%
  mean() %>%
  round(2) %>%
  "*"(100) %>%
  paste0("\\%") %>%
  write(file.path(stats_global_dir, "changes_district_r2.txt")) 

#### Top countries
q75_level <- acc_changes_best_df$r2 %>% quantile(probs = 0.75) %>% as.vector()
q75_level <- survey_chn_dist_df$r2 %>% quantile(probs = 0.75) %>% as.vector()

survey_chn_dist_df %>%
  pull(r2) %>%
  max() %>%
  round(2) %>%
  "*"(100) %>%
  paste0("\\%") %>%
  write(file.path(stats_global_dir, "changes_district_r2_max.txt")) 


acc_changes_best_df %>%
  dplyr::filter(r2 >= q75,
                income == "Low income") %>%
  pull(r2)

acc_changes_best_df %>%
  dplyr::filter(r2 >= q75,
                income == "Upper middle income") %>%
  pull(r2)

# Correlations -----------------------------------------------------------------
survey_df %>%
  dplyr::filter(!is.na(pca_allvars),
                !is.na(pca_allvars_mr)) %>%
  summarise(cor = cor(pca_allvars, pca_allvars_mr)) %>%
  pull(cor) %>%
  round(3) %>%
  write(file.path(stats_global_dir, "cor_levels_pca_allvars_mr_AND_pca_allvars.txt")) 

survey_df %>%
  dplyr::filter(!is.na(wealth_index),
                !is.na(pca_allvars_mr)) %>%
  group_by(country_code) %>%
  summarize(cor = cor(wealth_index, pca_allvars_mr)) %>%
  pull(cor) %>%
  mean() %>%
  round(3) %>%
  write(file.path(stats_global_dir, "cor_levels_pca_allvars_mr.txt")) 

survey_df %>%
  dplyr::filter(!is.na(wealth_index),
                !is.na(pca_allvars)) %>%
  group_by(country_code) %>%
  summarize(cor = cor(wealth_index, pca_allvars)) %>%
  pull(cor) %>%
  mean() %>%
  round(3) %>%
  write(file.path(stats_global_dir, "cor_levels_pca_allvars.txt")) 

survey_chn_df








