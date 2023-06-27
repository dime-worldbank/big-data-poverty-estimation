# PCA on CNN Features

DTL_SATELLITE <- "landsat"
SATELLITE <- 'ntlharmon'
UNDER_IA <- "True"
BANDS <- "rgb"

PER_VAR_EXPLAIN <- 0.99

for(DTL_SATELLITE in c("landsat", "s2")){
  for(SATELLITE in c("viirs", "ntlharmon")){
    for(UNDER_IA in c("True")){
      for(BANDS in c("rgb", "ndvi", "bu")){
        
        PATH_NAME <- paste0(DTL_SATELLITE, "_",SATELLITE,"_underia",UNDER_IA,"_b_",BANDS)
        
        print(PATH_NAME)
        
        if((DTL_SATELLITE == "landsat") & (SATELLITE == "viirs"))     next
        if((DTL_SATELLITE == "s2")      & (SATELLITE == "ntlharmon")) next
        
        if((SURVEY_NAME %in% "DHS_policy_experiment") & (DTL_SATELLITE == "s2")) next
        if((SURVEY_NAME %in% "DHS_policy_experiment") & (DTL_SATELLITE == "viirs")) next
        
        # Load data --------------------------------------------------------------------
        cnn_features_df <- file.path(data_dir,
                                     SURVEY_NAME,
                                     "FinalData",
                                     "Individual Datasets",
                                     "cnn_features",
                                     "split_into_data_subsets") %>%
          list.files(full.names = T) %>%
          str_subset(paste0("features_", PATH_NAME, "_")) %>%
          map_df(fread) %>%
          distinct(uid, .keep_all = T)
        
        ## Grab features (remove ID)
        cnn_feat_noid <- cnn_features_df %>%
          dplyr::select(-uid) %>%
          as.data.frame()
        
        ## Grab features that are non-constant
        cnn_feat_sd <- cnn_feat_noid %>%
          apply(2, sd) %>%
          as.numeric()
        
        cnn_feat_noid <- cnn_feat_noid[,cnn_feat_sd > 0]
        
        ## PCA and grab features that explain X% of variation
        pca_obj <- cnn_feat_noid %>%
          prcomp(scale = T)
        
        # Objsect with % variation explained
        pca_obj_sum <- summary(pca_obj) 
        
        use_features_tf <- as.numeric(pca_obj_sum$importance[3,]) < PER_VAR_EXPLAIN
        
        name_prefix <- paste0("cnn_" ,SATELLITE, "_", DTL_SATELLITE, "_", BANDS, "_")
        pca_df <- pca_obj$x[,use_features_tf] %>%
          as.data.frame() %>%
          rename_all(~tolower(paste0(name_prefix, .)))
        
        ## Add UID back in
        pca_df$uid <- cnn_features_df$uid %>% str_replace_all("b'", "") %>% str_replace_all("'", "")
        
        # Export -----------------------------------------------------------------------
        saveRDS(pca_df,
                (file.path(data_dir,
                           SURVEY_NAME,
                           'FinalData',
                           'Individual Datasets',
                           'cnn_features',
                           paste0('cnn_features_', PATH_NAME,'_pca.Rds'))))
        
      }
    }
  }
}

