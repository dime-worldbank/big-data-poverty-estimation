# Main Paper Stats

# Load data --------------------------------------------------------------------
survey_chn_df <- readRDS(file.path(data_dir, "DHS", "FinalData", "Merged Datasets", 
                                   "survey_alldata_clean_changes_cluster_predictions.Rds"))
survey_df     <- readRDS(file.path(data_dir, "DHS", "FinalData", "Merged Datasets", 
                                   "survey_alldata_clean_predictions.Rds"))
acc_df        <- readRDS(file.path(data_dir, "DHS", "FinalData", "pov_estimation_results",
                                   "accuracy_appended.Rds"))
acc_chng_dist_df <- readRDS(file.path(data_dir, "DHS", "FinalData", "pov_estimation_results",
                                      "accuracy_changes_district_appended.Rds"))

# N ----------------------------------------------------------------------------

#### Continents
survey_df %>%
  distinct(country_code, .keep_all = T) %>%
  pull(country_name) %>%
  countrycode(origin = "country.name",
              destination = "continent") %>%
  table()

survey_df %>%
  distinct(country_code, .keep_all = T) %>%
  pull(continent_adj) %>% 
  table()

#### N Clusters
survey_df %>%
  dplyr::filter(!is.na(predict_pca_allvars_mr_global_country_pred_all)) %>%
  nrow() %>%
  prettyNum(big.mark=",",scientific=FALSE) %>%
  write(file.path(stats_global_dir, "n_villages_levels.txt"))

#### N Countries
survey_df %>%
  dplyr::filter(!is.na(predict_pca_allvars_mr_global_country_pred_all)) %>%
  pull(country_code) %>%
  unique() %>%
  length() %>%
  write(file.path(stats_global_dir, "n_countries_levels.txt"))

#### N Countries
survey_chn_df %>%
  dplyr::filter(!is.na(predict_pca_allvars_best_all_changes)) %>%
  pull(country_code) %>%
  unique() %>%
  length() %>%
  write(file.path(stats_global_dir, "n_countries_changes.txt"))

#### N Features
survey_df %>%
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
                        starts_with("mosaik_"))) %>%
  ncol() %>%
  write(file.path(stats_global_dir, "n_features_levels.txt"))

# Main Results: Average, Min, Max ----------------------------------------------

##### * Cluster, Levels * #####

#### Overall r2
survey_df %>%
  dplyr::filter(most_recent_survey %in% T,
                !is.na(predict_pca_allvars_mr_global_country_pred_all)) %>%
  group_by(country_code) %>%
  dplyr::summarise(r2 = cor(predict_pca_allvars_mr_global_country_pred_all, pca_allvars_mr)^2) %>%
  ungroup() %>%
  pull(r2) %>%
  mean() %>%
  round(2) %>%
  "*"(100) %>%
  paste0("\\%") %>%
  write(file.path(stats_global_dir, "levels_village_r2.txt")) 

survey_df %>%
  dplyr::filter(most_recent_survey %in% T,
                !is.na(predict_pca_allvars_mr_global_country_pred_all)) %>%
  group_by(country_code) %>%
  dplyr::summarise(r2 = cor(predict_pca_allvars_mr_global_country_pred_all, pca_allvars_mr)^2) %>%
  ungroup() %>%
  pull(r2) %>%
  min() %>%
  round(2) %>%
  "*"(100) %>%
  paste0("\\%;") %>%
  write(file.path(stats_global_dir, "levels_village_r2_min.txt")) 

survey_df %>%
  dplyr::filter(most_recent_survey %in% T,
                !is.na(predict_pca_allvars_mr_global_country_pred_all)) %>%
  group_by(country_code) %>%
  dplyr::summarise(r2 = cor(predict_pca_allvars_mr_global_country_pred_all, pca_allvars_mr)^2) %>%
  ungroup() %>%
  pull(r2) %>%
  max() %>%
  round(2) %>%
  "*"(100) %>%
  paste0("\\%)") %>%
  write(file.path(stats_global_dir, "levels_village_r2_max.txt")) 

##### * District, Levels * #####

survey_df %>%
  dplyr::filter(most_recent_survey %in% T,
                !is.na(predict_pca_allvars_mr_global_country_pred_all)) %>%
  group_by(country_code, gadm_uid) %>%
  dplyr::summarise(predict_pca_allvars_mr_global_country_pred_all = mean(predict_pca_allvars_mr_global_country_pred_all),
                   pca_allvars_mr = mean(pca_allvars_mr)) %>%
  group_by(country_code) %>%
  dplyr::summarise(r2 = cor(predict_pca_allvars_mr_global_country_pred_all, pca_allvars_mr)^2) %>%
  ungroup() %>%
  pull(r2) %>%
  mean() %>%
  round(2) %>%
  "*"(100) %>%
  paste0("\\%") %>%
  write(file.path(stats_global_dir, "levels_district_r2_avg.txt"))

survey_df %>%
  dplyr::filter(most_recent_survey %in% T,
                !is.na(predict_pca_allvars_mr_global_country_pred_all)) %>%
  group_by(country_code, gadm_uid) %>%
  dplyr::summarise(predict_pca_allvars_mr_global_country_pred_all = mean(predict_pca_allvars_mr_global_country_pred_all),
                   pca_allvars_mr = mean(pca_allvars_mr)) %>%
  group_by(country_code) %>%
  dplyr::summarise(r2 = cor(predict_pca_allvars_mr_global_country_pred_all, pca_allvars_mr)^2) %>%
  ungroup() %>%
  pull(r2) %>%
  min() %>%
  round(2) %>%
  "*"(100) %>%
  paste0("\\%;") %>%
  write(file.path(stats_global_dir, "levels_district_r2_min.txt"))

survey_df %>%
  dplyr::filter(most_recent_survey %in% T,
                !is.na(predict_pca_allvars_mr_global_country_pred_all)) %>%
  group_by(country_code, gadm_uid) %>%
  dplyr::summarise(predict_pca_allvars_mr_global_country_pred_all = mean(predict_pca_allvars_mr_global_country_pred_all),
                   pca_allvars_mr = mean(pca_allvars_mr)) %>%
  group_by(country_code) %>%
  dplyr::summarise(r2 = cor(predict_pca_allvars_mr_global_country_pred_all, pca_allvars_mr)^2) %>%
  ungroup() %>%
  pull(r2) %>%
  max() %>%
  round(2) %>%
  "*"(100) %>%
  paste0("\\%)") %>%
  write(file.path(stats_global_dir, "levels_district_r2_max.txt"))

##### * Cluster, Changes * #####

#### Overall r2
survey_chn_df %>%
  dplyr::filter(!is.na(predict_pca_allvars_global_country_pred_all_changes)) %>%
  group_by(country_code) %>%
  dplyr::summarise(r2 = cor(predict_pca_allvars_global_country_pred_all_changes, pca_allvars)^2) %>%
  ungroup() %>%
  pull(r2) %>%
  mean() %>%
  round(2) %>%
  "*"(100) %>%
  paste0("\\%") %>%
  write(file.path(stats_global_dir, "changes_village_r2.txt")) 

survey_chn_df %>%
  dplyr::filter(!is.na(predict_pca_allvars_global_country_pred_all_changes)) %>%
  group_by(country_code) %>%
  dplyr::summarise(r2 = cor(predict_pca_allvars_global_country_pred_all_changes, pca_allvars)^2) %>%
  ungroup() %>%
  pull(r2) %>%
  min() %>%
  round(2) %>%
  "*"(100) %>%
  paste0("\\%;") %>%
  write(file.path(stats_global_dir, "changes_village_r2_min.txt")) 

survey_chn_df %>%
  dplyr::filter(!is.na(predict_pca_allvars_global_country_pred_all_changes)) %>%
  group_by(country_code) %>%
  dplyr::summarise(r2 = cor(predict_pca_allvars_global_country_pred_all_changes, pca_allvars)^2) %>%
  ungroup() %>%
  pull(r2) %>%
  median() %>%
  round(2) %>%
  "*"(100) %>%
  paste0("\\%;") %>%
  write(file.path(stats_global_dir, "changes_village_r2_median.txt")) 

survey_chn_df %>%
  dplyr::filter(!is.na(predict_pca_allvars_global_country_pred_all_changes)) %>%
  group_by(country_code) %>%
  dplyr::summarise(r2 = cor(predict_pca_allvars_global_country_pred_all_changes, pca_allvars)^2) %>%
  ungroup() %>%
  pull(r2) %>%
  max() %>%
  round(2) %>%
  "*"(100) %>%
  paste0("\\%)") %>%
  write(file.path(stats_global_dir, "changes_village_r2_max.txt")) 

survey_chn_df %>%
  dplyr::filter(!is.na(predict_pca_allvars_global_country_pred_all_changes)) %>%
  group_by(country_code) %>%
  dplyr::summarise(r2 = cor(predict_pca_allvars_global_country_pred_all_changes, pca_allvars)^2) %>%
  ungroup() %>%
  pull(r2) %>%
  max() %>%
  round(2) %>%
  "*"(100) %>%
  paste0("\\%") %>%
  write(file.path(stats_global_dir, "changes_village_r2_max_no_paren.txt")) 

##### * District, Changes * #####

acc_chng_dist_r2_df <- acc_chng_dist_df %>%
  dplyr::filter(feature_type %in% "all_changes",
                estimation_type %in% "global_country_pred") 

acc_chng_dist_r2_df %>% 
  dplyr::filter() %>%
  pull(r2) %>%
  mean() %>%
  round(2) %>%
  "*"(100) %>%
  paste0("\\%") %>%
  write(file.path(stats_global_dir, "changes_district_r2.txt"))

acc_chng_dist_r2_df %>% 
  dplyr::filter() %>%
  pull(r2) %>%
  min() %>%
  round(2) %>%
  "*"(100) %>%
  paste0("\\%;") %>%
  write(file.path(stats_global_dir, "changes_district_r2_min.txt"))

acc_chng_dist_r2_df %>% 
  dplyr::filter() %>%
  pull(r2) %>%
  max() %>%
  round(2) %>%
  "*"(100) %>%
  paste0("\\%)") %>%
  write(file.path(stats_global_dir, "changes_district_r2_max.txt"))

acc_chng_dist_r2_df %>% 
  dplyr::filter() %>%
  pull(r2) %>%
  max() %>%
  round(2) %>%
  "*"(100) %>%
  paste0("\\%") %>%
  write(file.path(stats_global_dir, "changes_district_r2_max_no_paren.txt"))

acc_chng_dist_r2_df %>% 
  dplyr::filter() %>%
  pull(r2) %>%
  median() %>%
  round(2) %>%
  "*"(100) %>%
  paste0("\\%") %>%
  write(file.path(stats_global_dir, "changes_district_r2_median.txt"))

# Model type that works best ---------------------------------------------------
est_type_best_df <- acc_df %>%
  dplyr::filter(feature_type == "all",
                estimation_type != "best") %>%
  group_by(country) %>%
  slice_max(order_by = r2, n = 1) %>%
  ungroup() %>%
  dplyr::mutate(within_country_best = estimation_type == "within_country_cv",
                within_country_best_num = as.numeric(within_country_best))

## Prop
est_type_best_vec <- est_type_best_df$estimation_type

est_type_best_vec %>% table()
round((est_type_best_vec %>% table()) / length(est_type_best_vec),2)
(est_type_best_vec %>% table()) / length(est_type_best_vec)

## N Cluster Comparison
est_type_best_df %>%
  dplyr::filter(estimation_type == "within_country_cv") %>%
  pull(n) %>% 
  quantile(c(0.5, 0.75))

est_type_best_df %>%
  dplyr::filter(estimation_type != "within_country_cv") %>%
  pull(n) %>% 
  quantile(c(0.5, 0.75))

# Compare results other papers -------------------------------------------------

##### * Yeh et al * #####

survey_af_df <- survey_df %>%
  dplyr::filter(most_recent_survey %in% T,
                continent_adj == "Africa")

# Pooled
survey_af_df %>%
  summarise(r2 = cor(pca_allvars_mr, predict_pca_allvars_mr_global_country_pred_all)^2) %>%
  round(2)

# Average Across Countries
survey_af_df %>%
  group_by(country_code) %>%
  summarise(r2 = cor(pca_allvars_mr, predict_pca_allvars_mr_global_country_pred_all)^2) %>%
  pull(r2) %>%
  mean() %>%
  round(2)

# Pooled - Urban
survey_af_df %>%
  dplyr::filter(urban_rural %in% "U") %>%
  summarise(r2 = cor(pca_allvars_mr, predict_pca_allvars_mr_global_country_pred_all)^2) %>%
  round(2)

# Pooled - Rural
survey_af_df %>%
  dplyr::filter(urban_rural %in% "R") %>%
  summarise(r2 = cor(pca_allvars_mr, predict_pca_allvars_mr_global_country_pred_all)^2) %>%
  round(2)

## District, Pooled
survey_af_df %>%
  group_by(country_code, gadm_uid) %>%
  dplyr::summarise(predict_pca_allvars_mr_global_country_pred_all = mean(predict_pca_allvars_mr_global_country_pred_all),
                   pca_allvars_mr = mean(pca_allvars_mr)) %>%
  ungroup() %>%
  dplyr::summarise(r2 = cor(predict_pca_allvars_mr_global_country_pred_all, pca_allvars_mr)^2) %>%
  round(2)

## District, Average Across Countries
survey_af_df %>%
  group_by(country_code, gadm_uid) %>%
  dplyr::summarise(predict_pca_allvars_mr_global_country_pred_all = mean(predict_pca_allvars_mr_global_country_pred_all),
                   pca_allvars_mr = mean(pca_allvars_mr)) %>%
  ungroup() %>%
  group_by(country_code) %>%
  dplyr::summarise(r2 = cor(predict_pca_allvars_mr_global_country_pred_all, pca_allvars_mr)^2) %>%
  pull(r2) %>%
  mean() %>%
  round(2)

## Changes, pooled
survey_chn_df %>%
  dplyr::filter(continent_adj %in% "Africa") %>%
  dplyr::summarise(r2 = cor(pca_allvars, predict_pca_allvars_global_country_pred_all_changes)^2) %>%
  round(2)

## Changes, district, pooled
survey_chn_df %>%
  dplyr::filter(continent_adj %in% "Africa") %>%
  
  group_by(gadm_uid, country_code) %>%
  dplyr::summarise(pca_allvars = mean(pca_allvars),
                   predict_pca_allvars_global_country_pred_all_changes = mean(predict_pca_allvars_global_country_pred_all_changes)) %>%
  ungroup() %>%
  
  dplyr::summarise(r2 = cor(pca_allvars, predict_pca_allvars_global_country_pred_all_changes)^2) %>%
  round(2)

##### * Chi et al * #####

survey_df %>%
  dplyr::filter(most_recent_survey %in% T) %>%
  group_by(country_code) %>%
  dplyr::summarise(r2 = cor(predict_pca_allvars_mr_global_country_pred_all, pca_allvars_mr)^2) %>%
  ungroup() %>%
  pull(r2) %>%
  mean() %>%
  round(2)

##### * Jean et al * #####

acc_df %>%
  dplyr::filter(feature_type %in% "all",
                level_change %in% "levels",
                estimation_type %in% "global_country_pred",
                country_name %in% c("Uganda",
                                    "Tanzania",
                                    "Nigeria",
                                    "Rwanda",
                                    "Malawi")) %>%
  mutate(r2 = round(r2, 2)) %>%
  dplyr::select(country_name, r2)

# OLD ==========================================================================
# OLD ==========================================================================
# OLD ==========================================================================
# OLD ==========================================================================
# OLD ==========================================================================
# OLD ==========================================================================





# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# survey_chn_dist_df <- survey_chn_df %>%
#   dplyr::filter(!is.na(predict_pca_allvars_best_all_changes)) %>%
#   group_by(country_code, gadm_uid) %>%
#   dplyr::summarise(predict_pca_allvars_best_all_changes = mean(predict_pca_allvars_best_all_changes),
#                    pca_allvars = mean(pca_allvars)) %>%
#   group_by(country_code) %>%
#   dplyr::summarise(r2 = cor(predict_pca_allvars_best_all_changes, pca_allvars)^2)
# 
# 
# 
# 
# 
# 
# 
# 
# acc_levels_best_df <- acc_df %>%
#   dplyr::filter(feature_type %in% "all",
#                 estimation_type %in% "best",
#                 target_var_dep %in% "pca_allvars_mr",
#                 level_change %in% "levels")
# 
# acc_changes_best_df <- acc_df %>%
#   dplyr::filter(feature_type %in% "all_changes",
#                 estimation_type %in% "best",
#                 target_var_dep %in% "pca_allvars",
#                 level_change %in% "changes")
# 
# # Create datasets --------------------------------------------------------------
# 
# 
# # Stats: Levels Results --------------------------------------------------------
# 
# 
# survey_df %>%
#   dplyr::filter(!is.na(predict_pca_allvars_mr_global_country_pred_all)) %>%
#   group_by(country_code, gadm_uid) %>%
#   dplyr::summarise(predict_pca_allvars_mr_global_country_pred_all = mean(predict_pca_allvars_mr_global_country_pred_all),
#                    pca_allvars_mr = mean(pca_allvars_mr)) %>%
#   ungroup() %>%
#   dplyr::summarise(r2 = cor(predict_pca_allvars_mr_global_country_pred_all, pca_allvars_mr)^2) %>%
#   ungroup() %>%
#   pull(r2) %>%
#   mean() %>%
#   round(2) %>%
#   "*"(100) %>%
#   paste0("\\%") %>%
#   write(file.path(stats_global_dir, "levels_district_r2_pooled.txt"))
# 
# #### Low / High Income r2
# acc_levels_best_df %>%
#   dplyr::filter(income %in% "Low income") %>%
#   pull(r2) %>%
#   mean() %>%
#   round(2) %>%
#   "*"(100) %>%
#   paste0("\\%") %>%
#   write(file.path(stats_global_dir, "levels_low_income_r2.txt"))
# 
# #### Upper middle income r2
# acc_levels_best_df %>%
#   dplyr::filter(income %in% "Upper middle income") %>%
#   pull(r2) %>%
#   mean() %>%
#   round(2) %>%
#   "*"(100) %>%
#   paste0("\\%") %>%
#   write(file.path(stats_global_dir, "levels_uppermiddle_income_r2.txt"))
# 
# 
# 
# 
# 
# ## When within country doesn't work best
# est_type_best_df_within <- est_type_best_df %>%
#   dplyr::filter(within_country_best %in% T)
# 
# est_type_best_df_notwithin <- est_type_best_df %>%
#   dplyr::filter(within_country_best %in% F)
# 
# est_type_best_df_within$N_dhs_obs %>% summary()
# est_type_best_df_notwithin$N_dhs_obs %>% summary()
# 
# est_type_best_df_within$pca_allvars_mr_sd %>% summary()
# est_type_best_df_notwithin$pca_allvars_mr_sd %>% summary()
# 
# est_type_best_df_within$continent_adj %>% table()
# est_type_best_df_notwithin$continent_adj %>% table()
# 
# est_type_best_df_within$income %>% table()
# est_type_best_df_notwithin$income %>% table()
# 
# lm(within_country_best_num ~ log(N_dhs_obs), data = est_type_best_df) %>% summary()
# lm(within_country_best_num ~ pca_allvars_mr_sd, data = est_type_best_df) %>% summary()
# lm(within_country_best_num ~ viirs_avg_rad_sdspace, data = est_type_best_df) %>% summary()
# 
# mean(est_type_best_df_notwithin$N_dhs_obs < 399)
# 
# est_type_best_df %>%
#   ggplot() +
#   geom_boxplot(aes(x = within_country_best,
#                    y = viirs_avg_rad_sdspace))
# 
# est_type_best_df %>%
#   ggplot() +
#   geom_boxplot(aes(x = within_country_best,
#                    y = log(pca_allvars_mr_sd)))
# 
# # Stats: Results Changes ------------------------------------------------
# #### N Obs Changes, Villages
# survey_chn_df %>%
#   dplyr::filter(!is.na(predict_pca_allvars_best_all_changes)) %>%
#   nrow() %>%
#   write(file.path(stats_global_dir, "n_obs_changes_villages.txt"))
# 
# 
# 
# 
# survey_chn_df %>%
#   dplyr::filter(!is.na(predict_pca_allvars_best_all_changes)) %>%
#   group_by(country_code) %>%
#   dplyr::summarise(r2 = cor(predict_pca_allvars_best_all_changes, pca_allvars)^2) %>%
#   ungroup() %>%
#   pull(r2) %>%
#   median() %>%
#   round(2) %>%
#   write(file.path(stats_global_dir, "changes_village_r2_median_no_perc.txt")) 
# 
# survey_chn_df %>%
#   dplyr::filter(!is.na(predict_pca_allvars_best_all_changes)) %>%
#   group_by(country_code) %>%
#   dplyr::summarise(r2 = cor(predict_pca_allvars_best_all_changes, pca_allvars)^2) %>%
#   ungroup() %>%
#   pull(r2) %>%
#   max() %>%
#   round(2) %>%
#   "*"(100) %>%
#   paste0("\\%)") %>%
#   write(file.path(stats_global_dir, "changes_village_r2_max.txt")) 
# 
# 
# # survey_chn_df %>%
# #   dplyr::filter(!is.na(predict_pca_allvars_best)) %>%
# #   group_by(country_code, gadm_uid) %>%
# #   dplyr::summarise(predict_pca_allvars_best = mean(predict_pca_allvars_best),
# #                    pca_allvars = mean(pca_allvars)) %>%
# #   group_by(country_code) %>%
# #   dplyr::summarise(r2 = cor(predict_pca_allvars_best, pca_allvars)^2) %>%
# #   ungroup() %>%
# #   pull(r2) %>%
# #   mean() %>%
# #   round(2) %>%
# #   "*"(100) %>%
# #   paste0("\\%") %>%
# #   write(file.path(stats_global_dir, "changes_district_r2.txt")) 
# # 
# # #### Top countries
# # q75_level <- acc_changes_best_df$r2 %>% quantile(probs = 0.75) %>% as.vector()
# # q75_level <- survey_chn_dist_df$r2 %>% quantile(probs = 0.75) %>% as.vector()
# # 
# # survey_chn_dist_df %>%
# #   pull(r2) %>%
# #   max() %>%
# #   round(2) %>%
# #   "*"(100) %>%
# #   paste0("\\%") %>%
# #   write(file.path(stats_global_dir, "changes_district_r2_max.txt")) 
# 
# # acc_changes_best_df %>%
# #   dplyr::filter(r2 >= q75,
# #                 income == "Low income") %>%
# #   pull(r2)
# # 
# # acc_changes_best_df %>%
# #   dplyr::filter(r2 >= q75,
# #                 income == "Upper middle income") %>%
# #   pull(r2)
# 
# # Correlations -----------------------------------------------------------------
# survey_df %>%
#   dplyr::filter(!is.na(pca_allvars),
#                 !is.na(pca_allvars_mr)) %>%
#   summarise(cor = cor(pca_allvars, pca_allvars_mr)) %>%
#   pull(cor) %>%
#   round(3) %>%
#   write(file.path(stats_global_dir, "cor_levels_pca_allvars_mr_AND_pca_allvars.txt")) 
# 
# survey_df %>%
#   dplyr::filter(!is.na(wealth_index),
#                 !is.na(pca_allvars_mr)) %>%
#   group_by(country_code) %>%
#   summarize(cor = cor(wealth_index, pca_allvars_mr)) %>%
#   pull(cor) %>%
#   mean() %>%
#   round(3) %>%
#   write(file.path(stats_global_dir, "cor_levels_pca_allvars_mr.txt")) 
# 
# survey_df %>%
#   dplyr::filter(!is.na(wealth_index),
#                 !is.na(pca_allvars)) %>%
#   group_by(country_code) %>%
#   summarize(cor = cor(wealth_index, pca_allvars)) %>%
#   pull(cor) %>%
#   mean() %>%
#   round(3) %>%
#   write(file.path(stats_global_dir, "cor_levels_pca_allvars.txt")) 
# 
# # Facebook MAU, Country Level --------------------------------------------------
# fb_df <- readRDS(file.path(fb_marketing_dir, "FinalData", "country_level_mau", 
#                            "country_level_mau.Rds")) %>%
#   dplyr::rename(iso2 = country_iso2)
# 
# wdi_df <- readRDS(file.path(data_dir, "WDI", "FinalData", "wdi.Rds"))
# 
# fb_wdi_df <- fb_df %>%
#   left_join(wdi_df, by = "iso2") %>%
#   dplyr::mutate(prop_estimate_mau_1 = estimate_mau_1 / wdi_population) %>%
#   dplyr::select(prop_estimate_mau_1, iso2, country)
# 
# fb_wdi_df$prop_estimate_mau_1 %>% summary()
# 
# 
# head(fb_df)
# head(wdi_df)
# 
# # District changes results -----------------------------------------------------
# 
# dist_r2_df %>%
#   dplyr::filter(income %in% "Low income") %>%
#   pull(r2) %>%
#   mean() %>%
#   round(2) %>%
#   "*"(100) %>%
#   paste0("\\%") %>%
#   write(file.path(stats_global_dir, "changes_district_r2_lowincome.txt"))
# 
# dist_r2_df %>% 
#   dplyr::filter(income %in% "Lower middle income") %>%
#   pull(r2) %>%
#   mean() %>%
#   round(2) %>%
#   "*"(100) %>%
#   paste0("\\%") %>%
#   write(file.path(stats_global_dir, "changes_district_r2_lowermiddleincome.txt"))
# 
# dist_r2_df %>% 
#   dplyr::filter(income %in% "Upper middle income") %>%
#   pull(r2) %>%
#   mean() %>%
#   round(2) %>%
#   "*"(100) %>%
#   paste0("\\%") %>%
#   write(file.path(stats_global_dir, "changes_district_r2_uppermiddleincome.txt"))
# 
# dist_r2_df$r2 %>% mean()
# dist_r2_df$r2[dist_r2_df$income %in% "Low income"] %>% mean()
# dist_r2_df$r2[dist_r2_df$income %in% "Lower middle income"] %>% mean()
# dist_r2_df$r2[dist_r2_df$income %in% "Upper middle income"] %>% mean()
# 
# # Leave one out levels average -------------------------------------------------
# 
# acc_df %>%
#   dplyr::filter(feature_type %in% "all",
#                 estimation_type %in% "global_country_pred",
#                 target_var_dep %in% "pca_allvars_mr",
#                 level_change %in% "levels") %>%
#   pull(r2) %>%
#   mean()
# 
# survey_df_af <- survey_df[survey_df$continent_adj == "Africa",]
# cor.test(survey_df$predict_pca_allvars_mr_best_all,
#          survey_df$pca_allvars_mr)
# 
# cor.test(survey_df_af$predict_pca_allvars_mr_global_country_pred_all,
#          survey_df_af$pca_allvars_mr)
# 
# survey_df %>%
#   dplyr::filter(!is.na(predict_pca_allvars_mr_global_country_pred_all),
#                 continent_adj == "Africa") %>%
#   group_by(country_code, gadm_uid) %>%
#   dplyr::summarise(predict_pca_allvars_mr_global_country_pred_all = mean(predict_pca_allvars_mr_global_country_pred_all),
#                    pca_allvars_mr = mean(pca_allvars_mr)) %>%
#   ungroup() %>%
#   dplyr::summarise(r2 = cor(predict_pca_allvars_mr_global_country_pred_all, pca_allvars_mr)^2)
# 
# # Pooled R2: Africa, Urban vs Rural --------------------------------------------
# df <- readRDS(file.path(data_dir, "DHS", "FinalData", "Merged Datasets", 
#                         "survey_alldata_clean_predictions.Rds"))
# 
# # All
# df %>%
#   dplyr::filter(most_recent_survey == T) %>%
#   dplyr::summarise(r2 = cor(pca_allvars_mr, predict_pca_allvars_mr_global_country_pred_all)^2)
# 
# # By continent
# df %>%
#   dplyr::filter(most_recent_survey == T) %>%
#   group_by(continent_adj) %>%
#   dplyr::summarise(r2 = cor(pca_allvars_mr, predict_pca_allvars_mr_global_country_pred_all)^2)
# 
# # By continent & urban/rural
# df %>%
#   dplyr::filter(most_recent_survey == T) %>%
#   group_by(continent_adj, urban_rural) %>%
#   dplyr::summarise(r2 = cor(pca_allvars_mr, predict_pca_allvars_mr_global_country_pred_all)^2)
# 
# # District, by continent
# df %>%
#   dplyr::filter(most_recent_survey == T) %>%
#   group_by(continent_adj, country_name, gadm_uid) %>%
#   dplyr::summarise(pca_allvars_mr = mean(pca_allvars_mr),
#                    predict_pca_allvars_mr_global_country_pred_all = mean(predict_pca_allvars_mr_global_country_pred_all)) %>%
#   ungroup() %>%
#   group_by(continent_adj) %>%
#   dplyr::summarise(r2 = cor(pca_allvars_mr, predict_pca_allvars_mr_global_country_pred_all)^2)
# 
# 
# 
# 
