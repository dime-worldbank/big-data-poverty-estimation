# Add select predictions to survey

# Load Data --------------------------------------------------------------------
#### Load Survey data
survey_df <- readRDS(file.path(data_dir, SURVEY_NAME, "FinalData", "Merged Datasets", "survey_alldata_clean_changes_cluster.Rds"))

#### Load Accuracy Data
acc_df <- readRDS(file.path(data_dir, SURVEY_NAME, "FinalData", "pov_estimation_results",
                            "accuracy_appended_bestparam.Rds"))

# acc_df <- acc_df %>%
#   dplyr::filter(level_change %in% "changes",
#                 ml_model_type %in% "glmnet",
#                 glmnet_alpha %in% 1)

acc_df <- acc_df %>%
  dplyr::filter(level_change %in% "changes",
                ml_model_type %in% "xgboost")


#### Load Predictions
read_add_file <- function(path){
  df <- readRDS(path)
  df$path <- path
  return(df)
}
pred_df <- file.path(data_dir, SURVEY_NAME, "FinalData", "pov_estimation_results", "predictions") %>%
  list.files(pattern = "*.Rds",
             full.names = T) %>%
  str_subset("changes") %>%
  map_df(read_add_file) %>%
  dplyr::filter(level_change != "levels_changevars_ng") 

pred_df <- pred_df %>%
  dplyr::mutate(estimation_type = case_when(
    estimation_type %>% str_detect("continent_") & estimation_type %>% str_detect("country_pred") ~ "same_continent",
    estimation_type %in% "continent" ~ "other_continents",
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
                                            ml_model_type,
                                            glmnet_alpha,
                                            svm_svr_eps, 
                                            svm_cost,
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
  slice_max(order_by = cor, n = 1, with_ties = F) %>%
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
# for(var in names(survey_df)){
#   print(var)
#   survey_df[[var]] <- survey_df[[var]] %>% unlist()
# }

## TODO: Fix / see what's going on?
survey_df$predict_pca_allvars_best[survey_df$country_code %in% "LS"] <- 
  survey_df$predict_pca_allvars_best[survey_df$country_code %in% "LS"] / 1000

survey_df$predict_pca_allvars_best[survey_df$country_code %in% "TL"] <- 
  survey_df$predict_pca_allvars_best[survey_df$country_code %in% "TL"] / 100

saveRDS(survey_df,
        file.path(data_dir, SURVEY_NAME, "FinalData", "Merged Datasets", 
                  "survey_alldata_clean_changes_cluster_predictions.Rds"))



# df <- readRDS(file.path(data_dir, SURVEY_NAME, "FinalData", "Merged Datasets",
#                 "survey_alldata_clean_changes_cluster_predictions.Rds"))
# 
# a <- df %>%
#   group_by(country_code, gadm_uid) %>%
#   dplyr::summarise(pca_allvars = mean(pca_allvars),
#                    predict_pca_allvars_best = mean(predict_pca_allvars_best)) %>%
#   ungroup() %>%
# 
#   group_by(country_code) %>%
#   dplyr::summarise(r2 = cor(pca_allvars, predict_pca_allvars_best)^2) %>%
#   ungroup()
# 
# a$r2 %>% hist()


