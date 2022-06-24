# PCA on CNN Features

SATELLITE <- 'viirs'
BANDS <- "rgb"
UNDER_IA <- "False"
PER_VAR_EXPLAIN <- 0.9

for(SATELLITE in c("viirs")){
  for(UNDER_IA in c("False")){
    for(BANDS in c("rgb", "ndvi", "bu")){
      
      PATH_NAME <- paste0("landsat_",SATELLITE,"_underia",UNDER_IA,"_",BANDS)
      
      # Load data --------------------------------------------------------------------
      cnn_feat_rgb <- readRDS(file.path(data_dir,
                                        SURVEY_NAME,
                                        "FinalData",
                                        "Individual Datasets",
                                        "cnn_features",
                                        paste0("cnn_features_", PATH_NAME, ".Rds")))
      
      ## Grab features (remove ID)
      cnn_feat_noid <- cnn_feat_rgb %>%
        dplyr::select(-uid) %>%
        as.data.frame()
      
      ## Grab features that are non-constant
      cnn_feat_sd <- cnn_feat_noid %>%
        apply(2, sd) %>%
        as.numeric()
      
      cnn_feat_noid <- cnn_feat_noid[,cnn_feat_sd > 0]
      
      ## PCA and grab features that explain 90% of variation
      pca_obj <- cnn_feat_noid %>%
        prcomp(scale = T)
      
      # Objsect with % variation explained
      pca_obj_sum <- summary(pca_obj) 
      
      use_features_tf <- as.numeric(pca_obj_sum$importance[3,]) < PER_VAR_EXPLAIN
      
      name_prefix <- paste0("cnn_" ,SATELLITE, "_", BANDS, "_")
      pca_df <- pca_obj$x[,use_features_tf] %>%
        as.data.frame() %>%
        rename_all(~tolower(paste0(name_prefix, .)))
      
      ## Add UID back in
      pca_df$uid <- cnn_feat_rgb$uid %>% str_replace_all("b'", "") %>% str_replace_all("'", "")
      
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
