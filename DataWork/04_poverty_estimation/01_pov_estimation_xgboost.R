# Poverty Estimation Using XGBoost

### Parameters

# For each model / time xgBoost is run, implement a grid search to determine the
# optimal parameters. Otherwise, use same set of parameters across all settings.
# Implementing the grid search adds significant time to code but improves model
# performance
grid_search <- F

# Check if model has already been implemented. If TRUE, replace; if FALSE, skip.
REPLACE_IF_EXTRACTED <- F

# Define out path --------------------------------------------------------------
OUT_PATH <- file.path(data_dir, "DHS", "FinalData", "pov_estimation_results")

# Delete existing files --------------------------------------------------------
if(REPLACE_IF_EXTRACTED){
  print("Removing files!!!")
  Sys.sleep(10)
  
  files_to_rm <- file.path(OUT_PATH) %>% 
    list.files(full.names = T, pattern = "*.Rds", recursive = T)
  
  files_to_rm <- files_to_rm %>% str_subset("levels_changevars_ng")
  
  for(file_i in files_to_rm){
    file.remove(file_i)
  }
  
  print("Files removed!")
}

# Functions --------------------------------------------------------------------
grab_x_features <- function(df, 
                            feature_type_i){
  # Subset features of dataframe based on "feature_type_i"
  # --df: Dataframe
  # --feature_type_i: Character describing feature category (e.g., "all", "osm")
  
  ## Restrict features
  if( (feature_type_i %in% "all") | (feature_type_i %>% str_detect("all_not_"))){
    
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
    
    if(feature_type_i %>% str_detect("all_not_")){
      
      var_to_rm <- feature_type_i %>%
        str_replace_all("all_not_", "")
      
      X <- X %>%
        dplyr::select_at(vars(-starts_with(var_to_rm)))
      
    }
    
    X <- X %>%
      as.matrix()
    
  } else if(feature_type_i %in% "all_except_cnn"){
    
    X <- df %>%
      dplyr::select_at(vars(starts_with("viirs_"),
                            starts_with("s1_sar_"),
                            starts_with("fb_prop_"),
                            starts_with("fb_wp_prop"),
                            #starts_with("osm_"),
                            starts_with("gc_"),
                            starts_with("l7_"),
                            starts_with("elevslope_"),
                            starts_with("weather_"),
                            starts_with("worldclim_"),
                            starts_with("pollution_"))) %>%
      as.matrix()
    
  } else if(feature_type_i %in% "all_changes"){
    
    X <- df %>%
      dplyr::select_at(vars(starts_with("ntlharmon_"),
                            starts_with("cnn_ntlharmon_landsat_"),
                            starts_with("l7_"),
                            starts_with("gc_"),
                            starts_with("weather_"),
                            starts_with("pollution_aod_"))) %>%
      as.matrix()
    
  } else if(feature_type_i %in% "satellites"){
    X <- df %>%
      dplyr::select_at(vars(starts_with("l7_"),
                            starts_with("cnn_viirs_s2_"),
                            starts_with("viirs_"),
                            starts_with("mosaik_"))) %>%
      as.matrix()
  } else if(feature_type_i %in% "satellites_changes"){
    X <- df %>%
      dplyr::select_at(vars(starts_with("l7_"),
                            starts_with("cnn_ntlharmon_landsat_"),
                            starts_with("ntlharmon_"))) %>%
      as.matrix()
  } else if(feature_type_i %in% "landcover"){
    X <- df %>%
      dplyr::select_at(vars(starts_with("gc_"),
                            starts_with("elevslope_"))) %>%
      as.matrix()
  } else if(feature_type_i %in% "weatherclimate"){
    X <- df %>%
      dplyr::select_at(vars(starts_with("weather_"),
                            starts_with("worldclim_"))) %>%
      as.matrix()
  } else{
    X <- df %>%
      dplyr::select_at(vars(starts_with( paste0(feature_type_i, "_") ))) %>%
      as.matrix()
  }
  
  return(X) 
}

#### PARAMS
estimation_type_i <- "within_country_cv" #  "continent"
feature_type_i <- "all"
target_var_i <- "wealth_index_score"
country_i <- "PK"
grid_search <- T

run_model_xgboost <- function(df,
                              level_change,
                              estimation_type_i,
                              feature_type_i,
                              target_var_i,
                              country_i,
                              ml_model_type,
                              xg_max.depth,
                              xg_eta,
                              xg_nthread,
                              xg_nrounds,
                              xg_subsample,
                              xg_objective,
                              xg_min_child_weight){
  
  grid_search <- F # REMOVE CODE FOR THIS
  
  ## Set Target Var
  df$target_var <- df[[target_var_i]]
  
  df <- df %>%
    dplyr::filter(!is.na(target_var))
  
  results_folds_list <- lapply(unique(df$fold), function(fold_i){
    
    ## Separate into train and test set
    if(estimation_type_i %>% str_detect("country_pred")){
      df_train <- df[!(df$country_code %in% country_i),]
      df_test  <- df[df$country_code %in% country_i,]
    } else{
      df_train <- df[!(df$fold %in% fold_i),]
      df_test  <- df[df$fold %in% fold_i,]
    }
    
    ## X and Y
    X_train <- grab_x_features(df_train, feature_type_i)
    X_test  <- grab_x_features(df_test,  feature_type_i)
    
    y_train <- df_train$target_var
    y_test  <- df_test$target_var
    
    ## Run xgboost mode
    if(grid_search){
      # --------------
      xgb_grid = expand.grid(
        nrounds = c(100, 1000),
        max_depth = c(2, 4, 6, 8, 10),
        #max_depth = c(2, 10),
        eta = c(0.1, 0.01, 0.001, 0.0001),
        #eta = c(0.01),
        gamma = 1,
        colsample_bytree = 1,
        min_child_weight = c(1,2,5),
        #subsample = c(0.3, 0.5)
        subsample = c(0.5)
      )
      
      # pack the training control parameters
      xgb_trcontrol = trainControl(
        method = "cv",
        number = 3,  
        search = "grid",
        allowParallel = TRUE,
        verboseIter = FALSE,
        returnData = FALSE
      )
      
      # train the model for each parameter combination in the grid,
      # using CV to evaluate
      xgb_model = train(
        X_train, y_train,  
        trControl = xgb_trcontrol,
        tuneGrid = xgb_grid,
        method = "xgbTree"
      )
      
    } else{
      
      # early_stopping_rounds
      xgb_model <- xgboost(data = X_train, 
                           label = y_train, 
                           max.depth = xg_max.depth, 
                           eta = xg_eta, 
                           nthread = xg_nthread, 
                           nrounds = xg_nrounds, 
                           subsample = xg_subsample,
                           objective = xg_objective,
                           min_child_weight = xg_min_child_weight,
                           
                           # 10% of number of rounds, but at least 10
                           #early_stopping_rounds = max(ceiling(xg_nrounds*0.1), 10),
                           print_every_n = 10)
      
    }
    
    ## Predictions
    pred <- predict(xgb_model, X_test)
    saveRDS(xgb_model, MODEL_OUT)
    
    results_fold_df <- data.frame(truth = y_test,
                                  prediction = pred,
                                  uid = df_test$uid,
                                  country_code = df_test$country_code,
                                  fold = fold_i,
                                  level_change = level_change,
                                  estimation_type = estimation_type_i,
                                  feature_type = feature_type_i,
                                  target_var = target_var_i,
                                  country_group = country_i,
                                  ml_model_type = ml_model_type,
                                  xg_max.depth = xg_max.depth,
                                  xg_eta = xg_eta,
                                  xg_nthread = xg_nthread,
                                  xg_nrounds = xg_nrounds,
                                  xg_subsample = xg_subsample,
                                  xg_objective = xg_objective,
                                  xg_min_child_weight = xg_min_child_weight)
    
    ## Feature Importance
    # Don't run feature importance using viirs (only one variable)
    if(feature_type_i %in% "viirs"){
      feat_imp_fold_df <- NULL
    } else{
      
      if(grid_search){
        # Make feature importance dataframe outputted from caret similar to dataframe 
        # outputted by xgboost. varImp returns Overall, which is Gain. By default,
        # scales between 0 and 100; xgboost doesn't do that, so don't do here.
        # https://stackoverflow.com/questions/59632899/does-the-caret-varimp-wrapper-for-xgboost-xgbtree-use-xgboost-gain
        feat_imp_fold_df <- varImp(xgb_model, scale=FALSE)$importance
        feat_imp_fold_df$Feature <- row.names(feat_imp_fold_df)
        
        feat_imp_fold_df <- feat_imp_fold_df %>%
          dplyr::rename(Gain = Overall)
      } else{
        feat_imp_fold_df                 <- xgb.importance(model = xgb_model)
      }
      
      feat_imp_fold_df$level_change    <- level_change
      feat_imp_fold_df$fold            <- fold_i
      feat_imp_fold_df$level_change <- level_change
      feat_imp_fold_df$estimation_type <- estimation_type_i
      feat_imp_fold_df$target_var      <- target_var_i
      feat_imp_fold_df$country_group   <- country_i
      feat_imp_fold_df$ml_model_type = ml_model_type
      feat_imp_fold_df$xg_max.depth = xg_max.depth
      feat_imp_fold_df$xg_eta = xg_eta
      feat_imp_fold_df$xg_nthread = xg_nthread
      feat_imp_fold_df$xg_nrounds = xg_nrounds
      feat_imp_fold_df$xg_subsample = xg_subsample
      feat_imp_fold_df$xg_objective = xg_objective
      feat_imp_fold_df$xg_min_child_weight = xg_min_child_weight
    }
    
    ## Best Parameters
    if(grid_search){
      grid_results_fold_df <- xgb_model$results %>%
        dplyr::mutate(fold = fold_i,
                      level_change = level_change,
                      estimation_type = estimation_type_i,
                      target_var = target_var_i,
                      country_group = country_i)
    } else{
      grid_results_fold_df <- data.frame(NULL)
    }
    
    return(list(results_fold_df = results_fold_df,
                feat_imp_fold_df = feat_imp_fold_df,
                grid_results_fold_df = grid_results_fold_df))
  })
  
  results_df <- map_df(results_folds_list, function(x) x$results_fold_df)
  feat_imp_df <- map_df(results_folds_list, function(x) x$feat_imp_fold_df)
  grid_imp_df <- map_df(results_folds_list, function(x) x$grid_results_fold_df)
  
  return(list(results_df = results_df,
              feat_imp_df = feat_imp_df,
              grid_imp_df = grid_imp_df))
}


run_model_glmnet <- function(df,
                             level_change,
                             estimation_type_i,
                             feature_type_i,
                             target_var_i,
                             country_i,
                             ml_model_type,
                             glmnet_alpha){
  
  ## Set Target Var
  df$target_var <- df[[target_var_i]]
  
  df <- df %>%
    dplyr::filter(!is.na(target_var))
  
  results_folds_list <- lapply(unique(df$fold), function(fold_i){
    
    ## Separate into train and test set
    if(estimation_type_i %>% str_detect("country_pred")){
      df_train <- df[!(df$country_code %in% country_i),]
      df_test  <- df[df$country_code %in% country_i,]
    } else{
      df_train <- df[!(df$fold %in% fold_i),]
      df_test  <- df[df$fold %in% fold_i,]
    }
    
    ## X and Y
    X_train <- grab_x_features(df_train, feature_type_i)
    X_test  <- grab_x_features(df_test,  feature_type_i)
    
    y_train <- df_train$target_var
    y_test  <- df_test$target_var
    
    # Make complete
    X_train_comp <- complete.cases(X_train)
    X_test_comp <- complete.cases(X_test)
    
    X_train <- X_train[X_train_comp,]
    y_train <- y_train[X_train_comp]
    
    X_test <- X_test[X_test_comp,]
    y_test <- y_test[X_test_comp]
    
    ## Run model
    cv_model <- cv.glmnet(X_train, y_train, 
                          alpha = glmnet_alpha, 
                          nfolds = 5)
    
    # summarize chosen configuration 
    optimal_lambda <- cv_model$lambda.min
    
    # Predictions
    pred <- predict(cv_model, s = optimal_lambda, newx = X_test) %>% as.numeric()
    saveRDS(list(cv_model = cv_model,
                 optimal_lambda = optimal_lambda), MODEL_OUT)
    
    # Results dataset
    results_fold_df <- data.frame(truth = y_test,
                                  prediction = pred,
                                  uid = df_test$uid[X_test_comp],
                                  country_code = df_test$country_code[1],
                                  fold = fold_i,
                                  level_change = level_change,
                                  estimation_type = estimation_type_i,
                                  feature_type = feature_type_i,
                                  target_var = target_var_i,
                                  country_group = country_i,
                                  ml_model_type = ml_model_type,
                                  glmnet_alpha = glmnet_alpha)
    
    ## Feature importance (just grab coefficients)
    best_model <- glmnet(X_train, y_train, alpha = glmnet_alpha, lambda = optimal_lambda)
    
    coef_df <- best_model %>% 
      coef() %>%
      as.matrix() %>%
      as.data.frame() %>%
      mutate(s0 = abs(s0))
    
    coef_df$Feature <- row.names(coef_df)
    
    coef_df <- coef_df %>%
      dplyr::filter(Feature != "(Intercept)")
    
    coef_df$level_change    <- level_change
    coef_df$fold            <- fold_i
    coef_df$level_change    <- level_change
    coef_df$estimation_type <- estimation_type_i
    coef_df$target_var      <- target_var_i
    coef_df$country_group   <- country_i
    coef_df$ml_model_type   <- ml_model_type
    coef_df$glmnet_alpha    <- glmnet_alpha
    
    return(list(results_fold_df = results_fold_df,
                feat_imp_fold_df = coef_df))
  })
  
  results_df <- map_df(results_folds_list, function(x) x$results_fold_df)
  feat_imp_df <- map_df(results_folds_list, function(x) x$feat_imp_fold_df)
  
  return(list(results_df = results_df,
              feat_imp_df = feat_imp_df))
}

run_model_svm <- function(df,
                          level_change,
                          estimation_type_i,
                          feature_type_i,
                          target_var_i,
                          country_i,
                          ml_model_type,
                          svm_svr_eps,
                          svm_cost){
  
  ## Set Target Var
  df$target_var <- df[[target_var_i]]
  
  df <- df %>%
    dplyr::filter(!is.na(target_var))
  
  results_folds_list <- lapply(unique(df$fold), function(fold_i){
    
    ## Separate into train and test set
    if(estimation_type_i %>% str_detect("country_pred")){
      df_train <- df[!(df$country_code %in% country_i),]
      df_test  <- df[df$country_code %in% country_i,]
    } else{
      df_train <- df[!(df$fold %in% fold_i),]
      df_test  <- df[df$fold %in% fold_i,]
    }
    
    ## X and Y
    X_train <- grab_x_features(df_train, feature_type_i)
    X_test  <- grab_x_features(df_test,  feature_type_i)
    
    y_train <- df_train$target_var
    y_test  <- df_test$target_var
    
    # Make complete
    X_train_comp <- complete.cases(X_train)
    X_test_comp <- complete.cases(X_test)
    
    X_train <- X_train[X_train_comp,]
    y_train <- y_train[X_train_comp]
    
    X_test <- X_test[X_test_comp,]
    y_test <- y_test[X_test_comp]
    
    ## Run model
    svm_model <- LiblineaR(data = X_train, 
                           target = y_train, 
                           C = svm_cost,
                           svr_eps = svm_svr_eps,
                           type = 11)
    # 
    # svm_model = svm(y_train ~ X_train, 
    #                 kernel = svm_kernel, 
    #                 cost = svm_cost, 
    #                 scale = TRUE)
    
    # Predictions
    #pred <- predict(svm_model, X_test) %>% as.numeric()
    pred <- predict(svm_model, X_test)$predictions
    saveRDS(svm_model, MODEL_OUT)
    
    # Results dataset
    results_fold_df <- data.frame(truth = y_test,
                                  prediction = pred,
                                  uid = df_test$uid[X_test_comp],
                                  country_code = df_test$country_code[1],
                                  fold = fold_i,
                                  level_change = level_change,
                                  estimation_type = estimation_type_i,
                                  feature_type = feature_type_i,
                                  target_var = target_var_i,
                                  country_group = country_i,
                                  ml_model_type = ml_model_type,
                                  svm_svr_eps = svm_svr_eps,
                                  svm_cost = svm_cost)
    
    return(list(results_fold_df = results_fold_df))
  })
  
  results_df <- map_df(results_folds_list, function(x) x$results_fold_df)
  
  return(list(results_df = results_df,
              feat_imp_df = data.frame(NULL)))
}

# Implement --------------------------------------------------------------------
for(level_change in c("levels", "changes", "levels_changevars_ng")){ # "levels", "changes", "levels_changevars_ng"
  
  # Levels - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  if(level_change %in% c("levels")){
    
    #### Load data
    df <- readRDS(file.path(data_dir, "DHS", "FinalData", "Merged Datasets", 
                            "survey_alldata_clean.Rds"))
    
    df <- df %>%
      dplyr::filter(most_recent_survey %in% T)
    
    #### Define parameters
    feature_types_indiv <- c("viirs",
                             "cnn_viirs_s2_rgb", 
                             "cnn_viirs_s2_ndvi", 
                             "cnn_viirs_s2_bu", 
                             "cnn_viirs_s2",
                             "l7",
                             "fb",
                             "osm",
                             "pollution",
                             "s1_sar",
                             "mosaik",
                             
                             # Groupings
                             "landcover",
                             "weatherclimate",
                             "satellites") 
    
    feature_types_all_but_indiv <- paste0("all_not_", 
                                          c(feature_types_indiv, "viirs")) 
    
    feature_types <- c(feature_types_indiv,
                       #feature_types_all_but_indiv,
                       "all")
    
    estimation_type_vec <- c("global_country_pred",
                             "within_country_cv",
                             "continent_africa_country_pred",
                             "continent_americas_country_pred",
                             "continent_eurasia_country_pred", 
                             "continent") # "continent" means "other continents"
    
    target_vars_vec <- c("pca_allvars_mr") 
    
    countries_vec <- c("all", unique(df$country_code)) 
  }
  
  # Levels - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  if(level_change %in% c("levels_changevars_ng")){
    
    #### Load data
    df <- readRDS(file.path(data_dir, "DHS", "FinalData", "Merged Datasets", 
                            "survey_alldata_clean.Rds"))
    
    df <- df %>%
      dplyr::filter(most_recent_survey %in% T)
    
    # #### Define parameters
    # feature_types_indiv <- c("viirs",
    #                          "cnn_viirs_s2_rgb", 
    #                          "cnn_viirs_s2_ndvi", 
    #                          "cnn_viirs_s2_bu", 
    #                          "cnn_viirs_s2",
    #                          "l7",
    #                          "fb",
    #                          "osm",
    #                          "pollution",
    #                          "s1_sar",
    #                          "mosaik",
    #                          
    #                          # Groupings
    #                          "landcover",
    #                          "weatherclimate",
    #                          "satellites") 
    # 
    # feature_types_all_but_indiv <- paste0("all_not_", 
    #                                       c(feature_types_indiv, "viirs")) 
    
    feature_types <- c("all_changes")
    
    estimation_type_vec <- c("global_country_pred", "continent_africa_country_pred")
    
    target_vars_vec <- c("pca_allvars") 
    
    countries_vec <- c("NG") 
  }
  
  # Changes - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  if(level_change %in% "changes"){
    
    #### Load data
    df <- readRDS(file.path(data_dir, "DHS", "FinalData", "Merged Datasets", 
                            "survey_alldata_clean_changes_cluster.Rds"))
    
    #### Define parameters
    feature_types_indiv <- c("ntlharmon",
                             "cnn_ntlharmon_landsat_rgb", 
                             "cnn_ntlharmon_landsat_ndvi", 
                             "cnn_ntlharmon_landsat_bu", 
                             "cnn_ntlharmon_landsat",
                             "l7",
                             "gc",
                             "weather",
                             "pollution_aod",
                             
                             # Groupings
                             "satellites_changes") 
    
    feature_types_all_but_indiv <- paste0("all_not_", 
                                          c(feature_types_indiv)) 
    
    feature_types <- c(feature_types_indiv,
                       #feature_types_all_but_indiv,
                       "all_changes") 
    
    estimation_type_vec <- c("within_country_cv",
                             "global_country_pred",
                             "continent_africa_country_pred",
                             "continent_americas_country_pred",
                             "continent_eurasia_country_pred", 
                             "continent")  # "continent" means "other continents"
    
    target_vars_vec <- c("pca_allvars") 
    
    countries_vec <- c("all", unique(df$country_code)) 
  } 
  
  for(estimation_type_i in estimation_type_vec){
    for(target_var_i in target_vars_vec){
      for(feature_type_i in feature_types){
        for(country_i in countries_vec){
          
          ## XG Boost Parameters
          if(level_change %in% c("levels", "levels_changevars_ng")){
            
            # xgboost defauls
            xg_max.depth_params <- 6
            xg_eta_params <- 0.3
            xg_nrounds_params <- 50
            xg_subsample_params <- 1 
            xg_objective_params <- c("reg:squarederror")
            xg_min_child_weight_params <- 1
            
            # xg_max.depth_params <- c(2, 5, 6, 10) 
            # xg_eta_params <- c(0.3) 
            # xg_nrounds_params <- c(50) 
            # xg_subsample_params <- c(0.3, 0.6, 1) 
            # xg_objective_params <- c("reg:squarederror")
            # xg_min_child_weight_params <- c(1)
            
            # xg_max.depth_params <- c(2, 5, 10) 
            # xg_eta_params <- c(0.3)
            # xg_nrounds_params <- c(50)
            # xg_subsample_params <- c(0.3)
            # xg_objective_params <- c("reg:squarederror")
            # xg_min_child_weight_params <- c(1)
            
            
            
            # xg_max.depth_params <- c(2, 5, 6, 10, 15, 20) %>% rev()
            # xg_eta_params <- c(0.3, 0.6, 0.9) %>% rev()
            # xg_nrounds_params <- c(50, 100, 200, 300) %>% rev()
            # xg_subsample_params <- c(0.3, 0.6, 1) %>% rev()
            # xg_objective_params <- c("reg:squarederror")
            # xg_min_child_weight_params <- c(1)
          }
          
          if(level_change == "changes"){
            
            # xgboost defauls
            xg_max.depth_params <- 6
            xg_eta_params <- 0.3
            xg_nrounds_params <- 50
            xg_subsample_params <- 1 
            xg_objective_params <- c("reg:squarederror")
            xg_min_child_weight_params <- 1
            
            # xg_max.depth_params <- c(2, 5, 6, 10)
            # xg_eta_params <- c(0.3) 
            # xg_nrounds_params <- c(50) 
            # xg_subsample_params <- c(0.3, 0.6, 1) 
            # xg_objective_params <- c("reg:squarederror")
            # xg_min_child_weight_params <- c(1)
            
            # xg_max.depth_params <- c(2, 5) 
            # xg_eta_params <- c(0.3)
            # xg_nrounds_params <- c(50)
            # xg_subsample_params <- c(0.3)
            # xg_objective_params <- c("reg:squarederror")
            # xg_min_child_weight_params <- c(1)
            
            
            
            # xg_max.depth_params <- c(2, 5, 6, 10, 15, 20) 
            # xg_eta_params <- c(0.3, 0.6, 0.9) 
            # xg_nrounds_params <- c(50, 100, 200, 300) 
            # xg_subsample_params <- c(0.3, 0.6, 1) 
            # xg_objective_params <- c("reg:squarederror")
            # xg_min_child_weight_params <- c(1)
          }
          
          # Skip -----------------------------------------------------------------
          # Only implement within country on individual countries
          if((estimation_type_i == "within_country_cv") & (country_i == "all")) next
          
          # Only implement "country_pred" types on individual countries
          if((str_detect(estimation_type_i, "country_pred")) & (country_i == "all")) next
          
          # Only implement continent on all countries
          if((estimation_type_i == "continent") & (country_i != "all")) next
          
          # Only implement weath_score with individual countries for DHS
          if((target_var_i == "wealth_index_score") & 
             ("DHS" == "DHS") & 
             (estimation_type_i != "within_country_cv")) next
          
          # Skip: If continent, country_pred, must be in same continent ----------
          if(estimation_type_i == "continent_africa_country_pred"){
            continent_i <- df$continent_adj[df$country_code %in% country_i][1]
            if(continent_i != "Africa") next
          }
          
          if(estimation_type_i == "continent_americas_country_pred"){
            continent_i <- df$continent_adj[df$country_code %in% country_i][1]
            if(continent_i != "Americas") next
          }
          
          if(estimation_type_i == "continent_eurasia_country_pred"){
            continent_i <- df$continent_adj[df$country_code %in% country_i][1]
            if(continent_i != "Eurasia") next
          }
          
          # Skip: Results that dont analyze ----------------------
          # We only look at each feature type for "within_country_cv
          # (Maybe also include: global_country_pred)
          if(!(estimation_type_i %in% c("within_country_cv"))){
            if(!(feature_type_i %in% c("all_changes", "all"))){
              next
            }
          }
          
          # Loop through XG boost ----------------------------------------------
          for(ml_model_type in c("svm", "xgboost", "glmnet")){ # "glmnet", "xgboost"
            
            ## xgboost parameter
            for(xg_max.depth in xg_max.depth_params){ # 2,5,6,10
              for(xg_eta in xg_eta_params){ # 0.3,0.8
                for(xg_nthread in 4){
                  for(xg_nrounds in xg_nrounds_params){ # 50,100,500
                    for(xg_subsample in xg_subsample_params){ # 0.3,0.6,1
                      for(xg_objective in xg_objective_params){
                        for(xg_min_child_weight in xg_min_child_weight_params){
                          
                          ## glmnet parameters
                          for(glmnet_alpha in c(0, 1)){
                            
                            ## svm paremeters
                            for(svm_svr_eps in c(0.1)){
                              for(svm_cost in c(10)){
                                
                                # Define Out Paths -----------------------------------------------------
                                if(ml_model_type == "xgboost"){
                                  
                                  file_name_suffix <- paste0(level_change,"_",
                                                             estimation_type_i,"_",
                                                             country_i,"_",
                                                             target_var_i,"_",
                                                             feature_type_i, "_",
                                                             ml_model_type, "_",
                                                             xg_max.depth %>% str_replace_all("[:punct:]", ""), "_",
                                                             xg_eta %>% str_replace_all("[:punct:]", ""), "_",
                                                             xg_nthread %>% str_replace_all("[:punct:]", ""), "_",
                                                             xg_nrounds %>% str_replace_all("[:punct:]", ""), "_",
                                                             xg_subsample %>% str_replace_all("[:punct:]", ""), "_",
                                                             xg_objective %>% str_replace_all("[:punct:]", ""), 
                                                             xg_min_child_weight %>% str_replace_all("[:punct:]", ""), 
                                                             ".Rds")
                                }
                                
                                if(ml_model_type %in% c("glmnet")){
                                  file_name_suffix <- paste0(level_change,"_",
                                                             estimation_type_i,"_",
                                                             country_i,"_",
                                                             target_var_i,"_",
                                                             feature_type_i, "_",
                                                             ml_model_type, "_",
                                                             glmnet_alpha,
                                                             ".Rds")
                                }
                                
                                if(ml_model_type %in% c("svm")){
                                  file_name_suffix <- paste0(level_change,"_",
                                                             estimation_type_i,"_",
                                                             country_i,"_",
                                                             target_var_i,"_",
                                                             feature_type_i, "_",
                                                             ml_model_type, "_",
                                                             svm_svr_eps %>% str_replace_all("[:punct:]", "_"), "_",
                                                             svm_cost,
                                                             ".Rds")
                                }
                                
                                PRED_OUT <- file.path(OUT_PATH, "predictions", 
                                                      paste0("predictions_", file_name_suffix))
                                
                                FI_OUT <- file.path(OUT_PATH, "feature_importance", 
                                                    paste0("fi_", file_name_suffix))
                                
                                ACCURACY_OUT <- file.path(OUT_PATH, "accuracy", 
                                                          paste0("accuracy_", file_name_suffix))
                                
                                GRIDSEARCH_OUT <- file.path(OUT_PATH, "grid_search", 
                                                            paste0("gs_", file_name_suffix))
                                
                                MODEL_OUT <- file.path(OUT_PATH, "models", 
                                                       paste0("model_", file_name_suffix))
                                
                                # Check if file exists/ should run -------------------------------------
                                if(!file.exists(PRED_OUT)){
                                  
                                  print(paste(ml_model_type,
                                              level_change,
                                              estimation_type_i,
                                              target_var_i,
                                              feature_type_i,
                                              country_i,
                                              sep = " - "))
                                  
                                  # Subset Data & Define Fold ------------------------------------------
                                  # For defining the fold: 
                                  # -- If "country_pred" in "estimation_type_i" name, then have only one 
                                  # fold; the function will treat country_i as train and others as test
                                  # -- If "country_pred" not in name, need to define > 1 fold; all
                                  # observations will be in train and test at some point
                                  
                                  if(estimation_type_i == "within_country_cv"){
                                    df_traintest <- df %>%
                                      dplyr::filter(country_code %in% country_i) %>%
                                      dplyr::mutate(fold = within_country_fold)
                                  }
                                  
                                  if(estimation_type_i == "global_country_pred"){
                                    df_traintest <- df %>%
                                      dplyr::mutate(fold = "fold_1")
                                  }
                                  
                                  if(estimation_type_i == "continent_africa_country_pred"){
                                    df_traintest <- df %>%
                                      dplyr::filter((country_code %in% country_i) |
                                                      (continent_adj %in% "Africa")) %>%
                                      dplyr::mutate(fold = "fold_1")
                                  }
                                  
                                  if(estimation_type_i == "continent_americas_country_pred"){
                                    df_traintest <- df %>%
                                      dplyr::filter((country_code %in% country_i) |
                                                      (continent_adj %in% "Americas")) %>%
                                      dplyr::mutate(fold = "fold_1")
                                  }
                                  
                                  if(estimation_type_i == "continent_eurasia_country_pred"){
                                    df_traintest <- df %>%
                                      dplyr::filter((country_code %in% country_i) |
                                                      (continent_adj %in% "Eurasia")) %>%
                                      dplyr::mutate(fold = "fold_1")
                                  }
                                  
                                  # Train on all countries in continent x and predict on countries in continent y
                                  if(estimation_type_i == "continent"){
                                    df_traintest <- df %>%
                                      dplyr::mutate(fold = continent_adj)
                                  }
                                  
                                  # Run Model ----------------------------------------------------------
                                  if(ml_model_type == "xgboost"){
                                    xg_results_list <- run_model_xgboost(df = df_traintest,
                                                                         level_change = level_change,
                                                                         estimation_type_i = estimation_type_i,
                                                                         feature_type_i = feature_type_i,
                                                                         target_var_i = target_var_i,
                                                                         country_i = country_i,
                                                                         ml_model_type = ml_model_type,
                                                                         xg_max.depth = xg_max.depth,
                                                                         xg_eta = xg_eta,
                                                                         xg_nthread = xg_nthread,
                                                                         xg_nrounds = xg_nrounds,
                                                                         xg_subsample = xg_subsample,
                                                                         xg_objective = xg_objective,
                                                                         xg_min_child_weight = xg_min_child_weight)
                                    
                                    results_df_i <- xg_results_list$results_df
                                  }
                                  
                                  if(ml_model_type %in% "glmnet"){
                                    
                                    xg_results_list <- run_model_glmnet(df = df_traintest,
                                                                        level_change = level_change,
                                                                        estimation_type_i = estimation_type_i,
                                                                        feature_type_i = feature_type_i,
                                                                        target_var_i = target_var_i,
                                                                        country_i = country_i,
                                                                        ml_model_type = ml_model_type,
                                                                        glmnet_alpha = glmnet_alpha)
                                    
                                    results_df_i <- xg_results_list$results_df
                                    
                                  }
                                  
                                  if(ml_model_type %in% "svm"){
                                    
                                    xg_results_list <- run_model_svm(df = df_traintest,
                                                                     level_change = level_change,
                                                                     estimation_type_i = estimation_type_i,
                                                                     feature_type_i = feature_type_i,
                                                                     target_var_i = target_var_i,
                                                                     country_i = country_i,
                                                                     ml_model_type = ml_model_type,
                                                                     svm_svr_eps = svm_svr_eps,
                                                                     svm_cost = svm_cost)
                                    
                                    results_df_i <- xg_results_list$results_df
                                    
                                  }
                                  
                                  # Accuracy Stats -----------------------------------------------------
                                  
                                  if(estimation_type_i == "continent"){
                                    acc_fold_df <- results_df_i %>%
                                      dplyr::group_by(country_code, fold) %>%
                                      dplyr::summarise(cor_fold = cor(truth, prediction),
                                                       coef_det_fold = coef_of_det(truth, prediction),
                                                       N_fold = n()) %>%
                                      ungroup()
                                  } else{
                                    acc_fold_df <- results_df_i %>%
                                      dplyr::group_by(fold) %>%
                                      dplyr::summarise(cor_fold = cor(truth, prediction),
                                                       coef_det_fold = coef_of_det(truth, prediction),
                                                       N_fold = n()) %>%
                                      ungroup()
                                  }
                                  
                                  acc_fold_df$cor_all <- cor(results_df_i$truth,
                                                             results_df_i$prediction)
                                  
                                  acc_fold_df$coef_det_all <- coef_of_det(results_df_i$truth,
                                                                          results_df_i$prediction)
                                  
                                  acc_fold_df <- acc_fold_df %>%
                                    dplyr::mutate(level_change = level_change,
                                                  estimation_type = estimation_type_i,
                                                  feature_type = feature_type_i,
                                                  target_var = target_var_i,
                                                  country = country_i,
                                                  ml_model_type = ml_model_type,
                                                  xg_max.depth = xg_max.depth,
                                                  xg_eta = xg_eta,
                                                  xg_nthread = xg_nthread,
                                                  xg_nrounds = xg_nrounds,
                                                  xg_subsample = xg_subsample,
                                                  xg_objective = xg_objective,
                                                  xg_min_child_weight = xg_min_child_weight,
                                                  glmnet_alpha = glmnet_alpha,
                                                  svm_svr_eps,
                                                  svm_cost,
                                                  n = nrow(df_traintest))
                                  
                                  # Export Results -----------------------------------------------------
                                  saveRDS(xg_results_list$feat_imp_df, FI_OUT)
                                  saveRDS(acc_fold_df,                 ACCURACY_OUT)
                                  saveRDS(xg_results_list$results_df,  PRED_OUT)
                                  
                                  # if(grid_search){
                                  #   saveRDS(xg_results_list$grid_imp_df, GRIDSEARCH_OUT)
                                  # }
                                  
                                  
                                }
                              }
                            }
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
}


