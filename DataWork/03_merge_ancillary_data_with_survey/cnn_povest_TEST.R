# Test CNN

# Load data --------------------------------------------------------------------
survey_df <- read.csv(file.path(data_dir, SURVEY_NAME, "FinalData",
                                            "Individual Datasets",
                                            "data_for_cnn.csv"),
                      stringsAsFactors = F)

cnn_rgb_df <- readRDS(file.path(INV_DATA_DIR, "cnn_features", 
                                "cnn_features_s2_rgb_pca.Rds"))

data_df <- cnn_rgb_df %>%
  left_join(survey_df, by = "uid")


xgb_grid = expand.grid(
  nrounds = c(100, 1000),
  max_depth = c(2, 4, 6, 8, 10),
  #max_depth = c(2, 10),
  eta = c(0.1, 0.01, 0.001, 0.0001),
  #eta = c(0.01),
  gamma = 1,
  colsample_bytree = 1,
  min_child_weight = c(1,2),
  #subsample = c(0.3, 0.5)
  subsample = c(0.5)
)

xgb_trcontrol = trainControl(
  method = "cv",
  number = 3,  
  search = "grid",
  allowParallel = TRUE,
  verboseIter = FALSE,
  returnData = FALSE
)

xgb_model = train(
  X_train, y_train,  
  trControl = xgb_trcontrol,
  tuneGrid = xgb_grid,
  method = "xgbTree"
)
