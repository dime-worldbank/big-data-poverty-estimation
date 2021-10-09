# Poverty Estimation Using XGBoost

# Load Data --------------------------------------------------------------------
SURVEY_NAME <- "DHS"
df <- readRDS(file.path(data_dir, SURVEY_NAME, "FinalData", "Merged Datasets", "survey_alldata_clean.Rds"))

OUT_PATH <- file.path(data_dir, SURVEY_NAME, "FinalData", "pov_estimation_results")

df$viirs_avg_rad <- log(df$viirs_avg_rad+1)

# Functions --------------------------------------------------------------------
grab_x_features <- function(df, 
                            feature_type_i){
  # Subset features of dataframe based on "feature_type_i"
  # --df: Dataframe
  # --feature_type_i: Character describing feature category (e.g., "all", "osm")
  
  ## Restrict features
  if(feature_type_i %in% "all"){
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
  } else if(feature_type_i %in% "satellites"){
    X <- df %>%
      dplyr::select_at(vars(contains("l8_"),
                            contains("viirs_"))) %>%
      as.matrix()
  } else{
    X <- df %>%
      dplyr::select_at(vars(contains( paste0(feature_type, "_") ))) %>%
      as.matrix()
  }
  
  return(X) 
}

#### PARAMS
estimation_type_i <- "within_country_cv"
feature_type_i <- "all"
target_var_i <- "wealth_index_score"
country_i <- "PK"

run_model <- function(df,
                      estimation_type_i,
                      feature_type_i,
                      target_var_i,
                      country_i){
  
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
    
    ## Run Model
    bstDense <- xgboost(data = X_train, 
                        label = y_train, 
                        max.depth = 5, 
                        eta = 0.1, 
                        nthread = 4, 
                        nrounds = 50, 
                        subsample = 0.3,
                        objective = "reg:squarederror")
    
    ## Predictions
    pred <- predict(bstDense, X_test)
    
    results_fold_df <- data.frame(truth = y_test,
                                  prediction = pred,
                                  uid = df_test$uid,
                                  fold = fold_i,
                                  estimation_type = estimation_type_i,
                                  feature_type = feature_type_i,
                                  target_var = target_var_i,
                                  country = country_i)
    
    ## Feature Importance
    feat_imp_fold_df                 <- xgb.importance(model = bstDense)
    feat_imp_fold_df$fold            <- fold_i
    feat_imp_fold_df$estimation_type <- estimation_type_i
    feat_imp_fold_df$target_var      <- target_var_i
    feat_imp_fold_df$country         <- country_i
    
    return(list(results_fold_df = results_fold_df,
                feat_imp_fold_df = feat_imp_fold_df))
  })
  
  results_df <- map_df(results_folds_list, function(x) x$results_fold_df)
  feat_imp_df <- map_df(results_folds_list, function(x) x$feat_imp_fold_df)
  
  return(list(results_df = results_df,
              feat_imp_df = feat_imp_df))
}

# Implement Functions ----------------------------------------------------------
feature_types <- c("all", "osm")

if(SURVEY_NAME == "DHS"){
  estimation_type_vec <- c("within_country_cv",
                           "global_country_pred",
                           "continent_africa_country_pred",
                           "continent_americas_country_pred",
                           "continent_eurasia_country_pred", 
                           "continent")
  target_vars_vec <- c("pca_allvars")
  countries_vec <- c("all", unique(df$country_code))
}

if(SURVEY_NAME == "OPM"){
  estimation_type_vec <- "within_country_cv"
  target_vars_vec <- c("pscores")
  countries_vec <- "PK"
}

for(estimation_type_i in estimation_type_vec){
  for(target_var_i in target_vars_vec){
    for(feature_type_i in feature_types){
      for(country_i in countries_vec){
        
        # Skip -----------------------------------------------------------------
        # Only implement within country on individual countries
        if((estimation_type_i == "within_country_cv") & (country_i == "all")) next
        
        # Only implement "country_pred" types on individual countries
        if((str_detect(estimation_type_i, "country_pred")) & (country_i == "all")) next
        
        # Only implement continent on all countries
        if((estimation_type_i == "continent") & (country_i != "all")) next
        
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
        
        # Define Out Paths -----------------------------------------------------
        file_name_suffix <- paste0(estimation_type_i,"_",
                                   country_i,"_",
                                   target_var_i,"_",
                                   feature_type_i,
                                   ".Rds")
        
        PRED_OUT <- file.path(OUT_PATH, "predictions", 
                              paste0("predictions_", file_name_suffix))
        
        FI_OUT <- file.path(OUT_PATH, "feature_importance", 
                            paste0("fi_", file_name_suffix))
        
        # Check if file exists/ should run -------------------------------------
        if(!file.exists(PRED_OUT) | REPLACE_IF_EXTRACTED){
          
          print(paste(estimation_type_i,
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
          run_model <- run_model(df = df_traintest,
                                 estimation_type_i = estimation_type_i,
                                 feature_type_i = feature_type_i,
                                 target_var_i = target_var_i,
                                 country_i = country_i)
          
          # Save Results -------------------------------------------------------
          saveRDS(run_model$results_df, PRED_OUT)
          saveRDS(run_model$feat_imp_df, FI_OUT)
        }
      }
    }
  }
}


