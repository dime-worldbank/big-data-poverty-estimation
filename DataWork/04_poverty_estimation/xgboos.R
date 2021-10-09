
library(xgboost)

SURVEY_NAME <- "DHS"
df <- readRDS(file.path(data_dir, SURVEY_NAME, "FinalData", "Merged Datasets", "survey_alldata_clean.Rds"))

# within_country_cv
# global
# continent_americas
# continent_eurasia
# 
est_type <- "within_country_cv"



df <- df %>%
  dplyr::filter(country_code %in% "KE")

df$viirs_avg_rad <- log(df$viirs_avg_rad+1)

X <- df %>%
  dplyr::select_at(vars(contains("osm_"),
                        #contains("fb_"),
                        contains("gc_"),
                        contains("weather_"),
                        contains("l8_"),
                        contains("elevslope_"),
                        contains("pollution_"),
                        contains("worldclim_"),
                        contains("viirs_"))) %>%
  as.matrix()
Y <- df$wealth_index_score

pred_df <- map_df(unique(df$within_country_fold), function(f){
  
  fold_i <- df$within_country_fold %in% f
  
  bstDense <- xgboost(data = X[!fold_i,], 
                      label = Y[!fold_i], 
                      max.depth = 5, 
                      eta = 0.1, 
                      nthread = 4, 
                      nrounds = 50, 
                      subsample = 0.3,
                      objective = "reg:squarederror")
  
  pred <- predict(bstDense, X[fold_i,])
  
  results_out <- data.frame(truth = Y[fold_i],
                            prediction = pred,
                            urban_rural = df$urban_rural[fold_i])
  
  feat_imp <- xgb.importance(model = bstDense)
  feat_imp$fold <- f
  
})

plot(pred_df$truth, pred_df$prediction)
cor(pred_df$truth, pred_df$prediction)^2



cor(pred_df$truth[pred_df$urban_rural %in% "R"], 
    pred_df$prediction[pred_df$urban_rural %in% "R"])^2

cor(pred_df$truth[pred_df$urban_rural %in% "U"], 
    pred_df$prediction[pred_df$urban_rural %in% "U"])^2


#plot(log(df$viirs_avg_rad+1), df$wealth_index_score)




