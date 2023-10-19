# Poverty Estimation Using XGBoost

### TODO
# 1. For OUT_MODEL, has "prediction" instead of "model"

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
  
  files_to_rm <- files_to_rm %>% str_subset("xgboost")
  
  for(file_i in files_to_rm){
    file.remove(file_i)
  }
  
  print("Files removed!")
}

# Make parameter grid ----------------------------------------------------------
xgb_grid = expand.grid(
  model = "xgboost",
  nrounds = c(50, 100), ## 50
  max_depth = c(6,2), ## 6
  eta = c(0.3), ## 0.3
  gamma = c(0), ##
  colsample_bytree = 1, ##
  min_child_weight = c(1), ## 1
  subsample = c(1, 0.5) ## 1
)

# xgb_grid = expand.grid(
#   model = "xgboost",
#   nrounds = c(50, 100), ## 50
#   max_depth = c(1,3,5,9), ## 6
#   eta = c(0.3), ## 0.3
#   gamma = c(0, 5), ##
#   colsample_bytree = c(0.9, 0.5), ##
#   min_child_weight = c(1), ## 1
#   subsample = c(0.9, 0.5) ## 1
# )

lambda_max <- 5
lambda_min <- 0.000001
K = 10
lambda_seq <- round(exp(seq(log(lambda_max),
                            log(lambda_min),
                            length.out = K)),
                    digits = 10)

glmnet_grid = expand.grid(
  model = "glmnet",
  glmnet_alpha = c(0, 0.5, 1),
  glmnet_lambda = lambda_seq
)

svm_grid <- expand.grid(
  model = "svm",
  svm_cost = c(1, 5, 10),
  svm_svr_eps = 0.1
)

# Functions --------------------------------------------------------------------
run_model <- function(param_i,
                      X_train,
                      y_train,
                      X_test){
  
  if(param_i$model == "xgboost"){
    model <- xgboost(data = X_train, 
                     label = y_train, 
                     max.depth = param_i$max_depth, 
                     eta = param_i$eta, 
                     nthread = 4, 
                     nrounds = param_i$nrounds, 
                     subsample = param_i$subsample,
                     objective = "reg:squarederror",
                     tree_method = "hist",
                     max_bin = 256,
                     min_child_weight = param_i$min_child_weight,
                     print_every_n = 10)
    
    pred <- predict(model, X_test)
  }
  
  ## Glmnet
  if(param_i$model == "glmnet"){
    
    glmnet_train_lambda <- F
    if(glmnet_train_lambda %in% T){
      
      ## Run model
      cv_model <- cv.glmnet(X_train, y_train, 
                            alpha = param_i$glmnet_alpha,
                            nfolds = 5)
      
      # Summarize chosen configuration 
      glmnet_best_lambda <- cv_model$lambda.min
      
      # Predictions
      pred <- predict(cv_model, s = glmnet_best_lambda, newx = X_test) %>% as.numeric()
      
      model <- list(cv_model = cv_model,
                    glmnet_best_lambda = glmnet_best_lambda)
      
    } else{
      ## Run model
      model <- glmnet(X_train, y_train, 
                      alpha = param_i$glmnet_alpha, 
                      lambda = param_i$glmnet_lambda)
      
      # Predictions
      pred <- predict(model, newx = X_test) %>% as.numeric()
    }
    
  }
  
  ## SVM
  if(param_i$model == "svm"){
    model <- LiblineaR(data = X_train, 
                       target = y_train, 
                       C = param_i$svm_cost,
                       svr_eps = param_i$svm_svr_eps,
                       type = 11)
    
    pred <- predict(model, X_test)$predictions
  }
  
  return(list(pred  = pred,
              model = model))
}

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
    
  } else if(feature_type_i %in% "all_lsms"){
    
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
                            starts_with("mosaik_"))) %>%
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

# Implement --------------------------------------------------------------------
for(level_change in c("levels", "changes", "nigeriaapplication", "lsms")){ 
  
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
    
    feature_types <- c(feature_types_indiv,
                       "all")
    
    estimation_type_vec <- c("global_country_pred",
                             "within_country_cv",
                             "continent", 
                             "other_continent")  
    
    target_vars_vec <- c("pca_allvars_mr") 
  }
  
  # Levels - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  if(level_change %in% "lsms"){
    
    #### Load data
    df <- readRDS(file.path(data_dir, "LSMS", "FinalData", "Merged Datasets", 
                            "survey_alldata_clean.Rds"))
    
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
    
    feature_types <- c(feature_types_indiv,
                       "all_lsms")
    
    estimation_type_vec <- c("within_country_cv",
                             "global_country_pred") 
    
    target_vars_vec <- c("pca_allvars_mr", "poverty_measure") 
  }
  
  # Levels - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  if(level_change %in% c("nigeriaapplication")){
    
    #### Load data
    df <- readRDS(file.path(data_dir, "DHS", "FinalData", "Merged Datasets", 
                            "survey_alldata_clean.Rds"))
    
    df <- df %>%
      dplyr::filter(most_recent_survey %in% T)
    
    feature_types <- c("all_changes")
    
    estimation_type_vec <- c("global_country_pred", "continent")
    
    target_vars_vec <- c("pca_allvars") 
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
                             "continent", 
                             "other_continent")  
    
    target_vars_vec <- c("pca_allvars") 
  } 
  
  for(estimation_type_i in rev(estimation_type_vec)){
    
    if(estimation_type_i %in% c("global_country_pred", 
                                "within_country_cv", 
                                "continent")){
      
      if(level_change %in% "nigeriaapplication"){
        countries_vec <- "NG"
      } else{
        countries_vec <- unique(df$country_code)
      }
      
    } else{
      countries_vec <- c("Africa", "Americas", "Eurasia")
    }
    
    for(model_type in c("xgboost", "glmnet", "svm")){ 
      for(target_var_i in rev(target_vars_vec)){
        for(feature_type_i in feature_types){
          for(country_i in rev(countries_vec)){
            
            # Skips ------------------------------------------------------------
            # Only train on individual feature types for global. So:
            # If not global and not "all" features, skip.
            if(level_change %in% c("levels", "changes")){
              if( (estimation_type_i != "global_country_pred") & (!(feature_type_i %in% c("all", "all_changes"))) ){
                next
              }
            }
            
            if( (level_change == "nigeriaapplication") & (model_type != "xgboost") ){
              next
            }
            
            if( (level_change == "lsms") & (model_type != "xgboost") ){
              next
            }
            
            # Parameter Grid ---------------------------------------------------
            if(model_type == "svm")      param_grid = svm_grid
            if(model_type == "xgboost")  param_grid = xgb_grid
            if(model_type == "glmnet")   param_grid = glmnet_grid
            
            # Define test folds to loop through ----------------------------------
            # For within country, we loop through folds; for others, we create
            # a dummy "folds_all" variable that is ignored.
            
            if(estimation_type_i %in% c("global_country_pred",
                                        "continent",
                                        "other_continent")){
              folds_all <- "onefold"
            }
            
            if(estimation_type_i %in% c("within_country_cv")){
              
              df_sub <- df[df$country_code %in% country_i,]
              folds_all <- unique(df_sub$within_country_fold)
            }
            
            for(fold_i in folds_all){
              
              # Make outputs -------------------------------------------------------
              OUT_BASE <- paste(level_change,
                                estimation_type_i,
                                target_var_i,
                                feature_type_i,
                                country_i,
                                model_type,
                                fold_i, 
                                sep = "_")
              
              OUT_PRED  <- file.path(OUT_PATH, "prediction", paste0("prediction_", OUT_BASE, ".Rds"))
              OUT_MODEL <- file.path(OUT_PATH, "model", paste0("prediction_", OUT_BASE, ".Rds"))
              
              if(!file.exists(OUT_PRED)){
                
                print(OUT_BASE)
                
                df$target_var <- df[[target_var_i]]
                
                # Prep train/test ------------------------------------------------
                if(estimation_type_i == "global_country_pred"){
                  df_train <- df[df$country_code != country_i,]
                  df_test  <- df[df$country_code == country_i,]
                }
                
                if(estimation_type_i == "continent"){
                  
                  continent_adj_i <- df$continent_adj[df$country_code == country_i][1]
                  
                  df_sub <- df[df$continent_adj == continent_adj_i,]
                  
                  df_train <- df_sub[df_sub$country_code != country_i,]
                  df_test  <- df_sub[df_sub$country_code == country_i,]
                }
                
                if(estimation_type_i == "other_continent"){
                  df_train <- df[df$continent_adj != country_i,]
                  df_test  <- df[df$continent_adj == country_i,]
                }
                
                if(estimation_type_i == "within_country_cv"){
                  df_sub <- df[df$country_code %in% country_i,]
                  
                  df_train <- df_sub[df_sub$within_country_fold != fold_i,]
                  df_test  <- df_sub[df_sub$within_country_fold == fold_i,]
                }
                
                # Check NA Values: Train ---------------------------------------
                # Remove rows that are all NA; not needed for XGboost
                if(model_type %in% c("glmnet", "svm")){
                  X_train_all_obs <- grab_x_features(df_train, feature_type_i)
                  has_non_na <- apply(X_train_all_obs, 1, function(x) T %in% !is.na(x) )
                  df_train <- df_train[has_non_na,]
                }
                
                # Remove rows with ANY NA
                if(model_type %in% c("svm")){
                  
                  X_train_all_obs <- grab_x_features(df_train, feature_type_i)
                  any_na_in_row <- apply(X_train_all_obs, 1, function(x) T %in% is.na(x)) %>% as.vector()
                  df_train <- df_train[!any_na_in_row,]
                }
                
                # Check NA Values: Test ---------------------------------------
                # Remove rows that are all NA; not needed for XGboost
                if(model_type %in% c("glmnet", "svm")){
                  X_train_all_obs <- grab_x_features(df_test, feature_type_i)
                  has_non_na <- apply(X_train_all_obs, 1, function(x) T %in% !is.na(x) )
                  df_test <- df_test[has_non_na,]
                }
                
                # Remove rows with ANY NA
                if(model_type %in% c("svm")){
                  
                  X_train_all_obs <- grab_x_features(df_test, feature_type_i)
                  any_na_in_row <- apply(X_train_all_obs, 1, function(x) T %in% is.na(x)) %>% as.vector()
                  df_test <- df_test[!any_na_in_row,]
                }
                
                # Prep folds -----------------------------------------------------
                if(estimation_type_i %in% c("global_country_pred",
                                            "continent",
                                            "other_continent")){
                  
                  ## Group into folds
                  df_train$fold_id <- paste(df_train$country_code, df_train$gadm_uid)
                  
                  fold_ids <- df_train$fold_id %>% unique() %>% sample()
                  
                  n_id_in_fold <- ceiling(length(fold_ids)/4)
                  
                  fold_group_ids <- split(fold_ids, ceiling(seq_along(fold_ids)/n_id_in_fold))
                  
                  df_train$fold <- NA
                  df_train$fold[df_train$fold_id %in% fold_group_ids$`1`] <- 1
                  df_train$fold[df_train$fold_id %in% fold_group_ids$`2`] <- 2
                  df_train$fold[df_train$fold_id %in% fold_group_ids$`3`] <- 3
                  df_train$fold[df_train$fold_id %in% fold_group_ids$`4`] <- 4
                }
                
                if(estimation_type_i == "within_country_cv"){
                  df_train$fold <- df_train$within_country_fold
                }
                
                # Choose Best Parameters -----------------------------------------
                
                ## Loop through parameters
                # param_grid_results_df <- map_df(1:nrow(param_grid), function(i){
                #   
                #   param_i <- param_grid[i,]
                #   
                #   ## Loop through folds, and take average R2 across folds
                #   param_grid_tmp_df <- map_df(unique(df_train$fold), function(fold_i){
                #     
                #     df_train_i <- df_train[df_train$fold != fold_i,]
                #     df_test_i  <- df_train[df_train$fold == fold_i,]
                #     
                #     X_train <- grab_x_features(df_train_i, feature_type_i)
                #     X_test  <- grab_x_features(df_test_i,  feature_type_i)
                #     
                #     y_train <- df_train_i$target_var
                #     y_test  <- df_test_i$target_var
                #     
                #     pred_df <- run_model(param_i,
                #                          X_train,
                #                          y_train,
                #                          X_test,
                #                          glmnet_train_lambda = T)
                #     
                #     R2_fold <- R2(pred_df$pred, y_test, form = "traditional")
                #     
                #     data.frame(R2_fold = R2_fold,
                #                glmnet_best_lambda = pred_df$glmnet_best_lambda)
                #     
                #   }) 
                #   
                #   data.frame(R2 = mean(param_grid_tmp_df$R2_fold),
                #              glmnet_best_lambda = mean(param_grid_tmp_df$glmnet_best_lambda)) 
                # 
                # }) 
                # 
                # param_grid$R2                 <- param_grid_results_df$R2
                # param_grid$glmnet_best_lambda <- param_grid_results_df$glmnet_best_lambda
                
                ## Loop through parameters
                param_grid$R2 <- lapply(1:nrow(param_grid), function(i){
                  
                  param_i <- param_grid[i,]
                  
                  ## Loop through folds, and take average R2 across folds
                  lapply(unique(df_train$fold), function(fold_i){
                    
                    df_train_i <- df_train[df_train$fold != fold_i,]
                    df_test_i  <- df_train[df_train$fold == fold_i,]
                    
                    X_train <- grab_x_features(df_train_i, feature_type_i)
                    X_test  <- grab_x_features(df_test_i,  feature_type_i)
                    
                    y_train <- df_train_i$target_var
                    y_test  <- df_test_i$target_var
                    
                    pred <- run_model(param_i,
                                      X_train,
                                      y_train,
                                      X_test)$pred
                    
                    R2_fold <- R2(pred, y_test, form = "traditional")
                    
                    R2_fold
                    
                  }) %>%
                    unlist() %>%
                    mean()
                }) %>%
                  unlist()
                
                # Train on all parameters ----------------------------------------
                param_grid_best <- param_grid %>%
                  dplyr::arrange(desc(R2)) %>%
                  head(1)
                
                ## Prediction
                X_train <- grab_x_features(df_train, feature_type_i)
                X_test  <- grab_x_features(df_test,  feature_type_i)
                
                y_train <- df_train$target_var
                y_test  <- df_test$target_var
                
                model_output <- run_model(param_grid_best,
                                          X_train,
                                          y_train,
                                          X_test)
                pred        <- model_output$pred
                model_final <- model_output$model
                
                #### Prediction Output
                pred_df <- df_test %>%
                  dplyr::select(uid, country_code, country_name, target_var)
                
                pred_df$prediction      <- pred
                pred_df$estimation_type <- estimation_type_i
                pred_df$feature_type    <- feature_type_i
                pred_df$target_var_dep  <- target_var_i
                pred_df$level_change    <- level_change
                pred_df$location        <- country_i
                pred_df$fold_i          <- fold_i
                
                # Export ---------------------------------------------------------
                saveRDS(model_final, OUT_MODEL)
                saveRDS(pred_df, OUT_PRED)
                
              }
            }
          }
        }
      }
    }
  }
}
