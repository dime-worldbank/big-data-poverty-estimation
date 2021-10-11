# PCA on CNN Features

SATELLITE = 'l8'

# Load data --------------------------------------------------------------------
cnn_feat_rgb <- fread(file.path(data_dir,
                                SURVEY_NAME,
                                'FinalData',
                                'Individual Datasets',
                                'cnn_features',
                                paste0('cnn_features_', SATELLITE, '_rgb.csv')))

cnn_feat_noid <- cnn_feat_rgb %>%
  dplyr::select(-uid) %>%
  as.data.frame()

cnn_feat_sd <- cnn_feat_noid %>%
  apply(2, sd) %>%
  as.numeric()

cnn_feat_noid <- cnn_feat_noid[,cnn_feat_sd > 0]

pca_obj <- cnn_feat_noid %>%
  prcomp(scale = T)


#### PCA HERE

a <- cnn_feat_rgb %>%
  dplyr::select_at(vars(contains("cnn_feat")))

aa <- a[1,] %>% as.vector() %>% as.numeric()
table(aa!=0)

saveRDS(cnn_feat_rgb,
        (file.path(data_dir,
                   SURVEY_NAME,
                   'FinalData',
                   'Individual Datasets',
                   'cnn_features',
                   paste0('cnn_features_', SATELLITE, '_rgb_pca.csv'))))


