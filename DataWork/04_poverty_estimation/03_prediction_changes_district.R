# District Level Changes

# Load Predictions  survey  ----------------------------------------------------
#### Predictions
read_add_file <- function(path){
  df <- readRDS(path)
  df$path <- path
  return(df)
}
pred_df <- file.path(data_dir, SURVEY_NAME, "FinalData", "pov_estimation_results", "predictions") %>%
  list.files(pattern = "*.Rds",
             full.names = T) %>%
  str_subset("changes") %>%
  map_df(read_add_file) 

#### Survey
survey_df <- readRDS(file.path(data_dir, SURVEY_NAME, "FinalData", "Merged Datasets", "survey_alldata_clean_changes_cluster.Rds"))
survey_df <- survey_df %>%
  dplyr::select(uid, gadm_uid, continent_adj, country_code, country_name, iso2)

# Cleanup ----------------------------------------------------------------------
pred_df <- pred_df %>%
  dplyr::mutate(estimation_type = case_when(
    estimation_type %>% str_detect("continent_") & estimation_type %>% str_detect("country_pred") ~ "same_continent",
    estimation_type %in% "continent" ~ "other_continents",
    TRUE ~ estimation_type
  )) 

# continent_df <- survey_df %>%
#   distinct(continent_adj, country_code)
# 
# pred_df <- pred_df %>%
#   left_join(continent_df, by = "country_code")

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

pred_df <- pred_df %>%
  left_join(survey_df, by = c("uid", "country_code"))

# Aggregate to district --------------------------------------------------------
pred_district_df <- pred_df %>%
  group_by(country_code, country_name, iso2, continent_adj, gadm_uid, country_model_param) %>%
  dplyr::summarise(prediction = mean(prediction),
                   truth = mean(truth)) %>%
  ungroup()

pred_district_df <- pred_district_df %>%
  group_by(country_model_param) %>%
  dplyr::mutate(cor = cor(prediction, truth),
                r2 = cor^2) %>%
  ungroup()
  
pred_district_best_sum_df <- pred_district_df %>%
  group_by(country_code) %>%
  slice_max(n = 1, order_by = cor, with_ties = FALSE) %>%
  ungroup()
  
pred_district_best_df <- pred_district_df[pred_district_df$country_model_param %in% pred_district_best_sum_df$country_model_param,]

# Merge WDI Indicators ---------------------------------------------------------
wdi_df <- readRDS(file.path(data_dir, "WDI", "FinalData", "wdi.Rds"))

wdi_df <- wdi_df %>%
  dplyr::select(-c(iso3c, country, year, capital, longitude, latitude))

pred_district_best_df <- pred_district_best_df %>%
  left_join(wdi_df, by = "iso2") 

# Export -----------------------------------------------------------------------
saveRDS(pred_district_best_df,
        file.path(data_dir, SURVEY_NAME, "FinalData", "Merged Datasets", "survey_alldata_clean_changes_cluster_predictions_district.Rds"))


# 
# 
# pred_district_best_df %>%
#   ggplot(aes(x = truth, y = prediction)) +
#   geom_point() +
#   facet_wrap(~country_code,
#              scales = "free")
# 

