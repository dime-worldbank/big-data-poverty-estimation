# Add select predictions to survey

# Load Data --------------------------------------------------------------------
#### Load Survey data
survey_df <- readRDS(file.path(data_dir, SURVEY_NAME, "FinalData", "Merged Datasets", "survey_alldata_clean_changes_cluster.Rds"))

#### Load Accuracy Data
acc_df <- readRDS(file.path(data_dir, SURVEY_NAME, "FinalData", "pov_estimation_results",
                            "accuracy_appended_bestparam.Rds"))

acc_df <- acc_df %>%
  dplyr::filter(level_change %in% "changes")

#### Load Predictions
pred_df <- file.path(data_dir, SURVEY_NAME, "FinalData", "pov_estimation_results", "predictions") %>%
  list.files(pattern = "*.Rds",
             full.names = T) %>%
  str_subset("changes") %>%
  map_df(readRDS) 

## DELETE LATER
# pred_df <- pred_df %>%
#   dplyr::filter(xg_max.depth %in% 5,
#                 xg_eta %in% 0.3,
#                 xg_nthread %in% 4,
#                 xg_nrounds %in% c(50, 100),
#                 xg_subsample %in% 0.3,
#                 xg_objective %in% "reg:squarederror",
#                 xg_min_child_weight %in% 1)

pred_df <- pred_df %>%
  dplyr::mutate(estimation_type = case_when(
    estimation_type %>% str_detect("continent_") ~ "other_continents",
    estimation_type %in% "continent" ~ "same_continent",
    TRUE ~ estimation_type
  ))

continent_df <- survey_df %>%
  distinct(continent_adj, country_code)

pred_df <- pred_df %>%
  left_join(continent_df, by = "country_code")

pred_df <- pred_df %>%
  dplyr::mutate(country_model_param = paste(country_code,
                                            estimation_type,
                                            target_var,
                                            feature_type,
                                            xg_max.depth,
                                            xg_eta,
                                            xg_nthread,
                                            xg_nrounds,
                                            xg_subsample,
                                            xg_objective,
                                            xg_min_child_weight,
                                            sep = "_")) 

# Restrict to best xg_parameters -----------------------------------------------
pred_df <- pred_df[pred_df$country_model_param %in% acc_df$country_model_param,]

# Cleanup prediction data ------------------------------------------------------
pred_long_df <- pred_df %>%
  dplyr::mutate(pred_var = paste("predict", target_var, estimation_type, feature_type, sep = "_")) 

# Dataset of best prediction for each country & target variable ----------------
pred_long_df <- pred_long_df %>%
  mutate(country_est_id = paste(pred_var, country_code))

cor_df <- pred_long_df %>%
  dplyr::filter(feature_type == "all_changes") %>%
  group_by(country_est_id, pred_var, target_var, country_code) %>%
  dplyr::summarise(cor = cor(truth, prediction)) %>%
  
  group_by(country_code, target_var) %>% 
  slice_max(order_by = cor, n = 1) %>%
  ungroup()

pred_long_best_df <- pred_long_df[pred_long_df$country_est_id %in% cor_df$country_est_id,]

# To wide ----------------------------------------------------------------------
## Best prediction for each country & target var, to wide
pred_wide_best_df <- pred_long_best_df %>%
  dplyr::mutate(pred_var = paste("predict", target_var, "best", sep = "_")) %>%
  pivot_wider(id_cols = uid,
              names_from = pred_var,
              values_from = prediction)

## All predictions, to wide
pred_wide_df <- pred_long_df %>%
  pivot_wider(id_cols = uid,
              names_from = pred_var,
              values_from = prediction)

# Merge back to survey ---------------------------------------------------------
survey_df <- survey_df %>%
  left_join(pred_wide_best_df, by = "uid") %>%
  left_join(pred_wide_df, by = "uid")

# Merge WDI Indicators ---------------------------------------------------------
wdi_df <- readRDS(file.path(data_dir, "WDI", "FinalData", "wdi.Rds"))

wdi_df <- wdi_df %>%
  dplyr::select(-c(iso3c, country, year, capital, longitude, latitude))

survey_df <- survey_df %>%
  left_join(wdi_df, by = "iso2") 

# Export -----------------------------------------------------------------------
saveRDS(survey_df,
        file.path(data_dir, SURVEY_NAME, "FinalData", "Merged Datasets", "survey_alldata_clean_changes_cluster_predictions.Rds"))

