# PCA on CNN Features

SATELLITE <- 'l8'
BANDS <- "rgb"
PER_VAR_EXPLAIN <- 0.9

# Load data --------------------------------------------------------------------
cnn_feat_rgb <- fread(file.path(data_dir,
                                SURVEY_NAME,
                                'FinalData',
                                'Individual Datasets',
                                'cnn_features',
                                paste0('cnn_features_', SATELLITE, '_',BANDS,'.csv')))

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

use_features_tf <- as.numeric(pc_df$importance[3,]) < PER_VAR_EXPLAIN

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
                   paste0('cnn_features_', SATELLITE, '_',BANDS,'_pca.Rds'))))


