# Predict Poverty for Points Across Pakistan

# Load data --------------------------------------------------------------------
dhs_df <- readRDS(file.path(data_dir, "DHS", "FinalData", "Merged Datasets", 
                            "survey_alldata_clean.Rds"))

# TODO: Edits this later
pakpoints_df <- readRDS(file.path(data_dir, "PAK_POINTS", "FinalData", "Individual Datasets", 
                                  "survey_socioeconomic.Rds"))

pakpoints_gc_df <- readRDS(file.path(data_dir, "PAK_POINTS", "FinalData", "Individual Datasets", 
                                     "globcover", "gc_PK_1500m_2020.Rds"))

pakpoints_df <- pakpoints_df %>%
  left_join(pakpoints_gc_df, by = c("uid", "year"))

# Prep data --------------------------------------------------------------------
dhs_df <- dhs_df %>%
  dplyr::filter(country_code %in% "PK")

X_train <- dhs_df %>%
  dplyr::select(contains("gc_")) %>%
  as.matrix()

y_train <- dhs_df$wealth_index_score

X_predict <- pakpoints_df %>%
  dplyr::select(contains("gc_")) %>%
  as.matrix()

# Train model & predict --------------------------------------------------------
xgb_model <- xgboost(data = X_train, 
                     label = y_train, 
                     max.depth = 5, 
                     eta = 0.1, 
                     nthread = 4, 
                     nrounds = 50, 
                     subsample = 0.3,
                     objective = "reg:squarederror",
                     print_every_n = 1000L)

pakpoints_df$predict_wealth_index_score <- predict(xgb_model, X_predict)

# Export -----------------------------------------------------------------------
saveRDS(pakpoints_df,
        file.path(data_dir, "PAK_POINTS", 
                  "FinalData", "Merged Datasets", "survey_alldata_clean_predictions.Rds"))








