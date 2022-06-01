# Add select predictions to survey

# TODO: To modify

# Load Data --------------------------------------------------------------------
survey_df <- readRDS(file.path(data_dir, SURVEY_NAME, "FinalData", "Merged Datasets", "survey_alldata_clean.Rds"))

# Load accuracy stats; either load best or specific param
file.path(data_dir, SURVEY_NAME, "FinalData", "pov_estimation_results") %>%
  list.files() %>%
  str_subset("accuracy_appended")
acc_df <- readRDS(file.path(data_dir, SURVEY_NAME, "FinalData", "pov_estimation_results",
                            "accuracy_appended_bestparam.Rds"))

pred_df <- file.path(data_dir, SURVEY_NAME, "FinalData", "pov_estimation_results", "predictions") %>%
  list.files(pattern = "*.Rds",
             full.names = T) %>%
  map_df(readRDS) 

# Restrict to best xg_parameters -----------------------------------------------
pred_df <- pred_df %>%
  dplyr::mutate(country_model_param = paste(country_code,
                                            estimation_type,
                                            feature_type,
                                            target_var,
                                            xg_max.depth,
                                            xg_eta,
                                            xg_nthread,
                                            xg_nrounds,
                                            xg_subsample,
                                            xg_objective,
                                            sep = "_"))

acc_df <- acc_df %>%
  dplyr::mutate(country_model_param = paste(country,
                                            estimation_type,
                                            feature_type,
                                            target_var,
                                            xg_max.depth,
                                            xg_eta,
                                            xg_nthread,
                                            xg_nrounds,
                                            xg_subsample,
                                            xg_objective,
                                            sep = "_"))

pred_df <- pred_df[pred_df$country_model_param %in% acc_df$country_model_param,]

# Cleanup prediction data ------------------------------------------------------
pred_long_df <- pred_df %>%
  dplyr::mutate(estimation_type = case_when(
    estimation_type %>% str_detect("continent_") ~ "other_continents",
    TRUE ~ estimation_type
  )) %>%
  dplyr::mutate(pred_var = paste("predict", target_var, estimation_type, feature_type, sep = "_")) 

# Dataset of best prediction for each country & target variable ----------------
pred_long_df <- pred_long_df %>%
  mutate(country_est_id = paste(pred_var, country_code))

cor_df <- pred_long_df %>%
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

# Export -----------------------------------------------------------------------
saveRDS(survey_df,
        file.path(data_dir, SURVEY_NAME, "FinalData", "Merged Datasets", "survey_alldata_clean_predictions.Rds"))




