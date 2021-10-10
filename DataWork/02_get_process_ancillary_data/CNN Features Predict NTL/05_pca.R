# PCA on CNN Features

SATELLITE = 'l8'

# Load data --------------------------------------------------------------------
cnn_feat_rgb <- fread(file.path(data_dir,
                                SURVEY_NAME,
                                'FinalData',
                                'Individual Datasets',
                                'cnn_features',
                                paste0('cnn_features_', SATELLITE, '_rgb.csv')))

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


