# Move data from Google Drive to Dropbox

SATELLITE <- 's2'
BANDS <- "rgb"

for(BANDS in c("rgb", "ndvi", "bu")){
  
  print(paste(BANDS, "-------------------------------------------------------"))
  
  # Features -------------------------------------------------------------------
  cnn_features_df <- fread(file.path(gdrive_dir,
                                     "Data",
                                     SURVEY_NAME,
                                     "FinalData",
                                     "cnn_features",
                                     paste0('features_', SATELLITE, '_b_',BANDS,'.csv')))
  
  cnn_features_df <- cnn_features_df %>% as.data.frame()
  
  saveRDS(cnn_features_df,
          file.path(data_dir,
                    SURVEY_NAME,
                    "FinalData",
                    "Individual Datasets",
                    "cnn_features",
                    paste0("cnn_features_", SATELLITE, "_", BANDS, ".Rds")))
  
  # Predictions ----------------------------------------------------------------
  cnn_predictions_df <- fread(file.path(gdrive_dir,
                                        "Data",
                                        SURVEY_NAME,
                                        "FinalData",
                                        "cnn_predictions",
                                        paste0('predictions', SATELLITE, '_',BANDS,'.csv')))
  
  cnn_predictions_df$uid <- cnn_predictions_df$uid %>%  str_replace_all("b'", "") %>% str_replace_all("'", "")
  
  saveRDS(cnn_predictions_df,
          file.path(data_dir,
                    SURVEY_NAME,
                    "FinalData",
                    "Individual Datasets",
                    "cnn_features",
                    paste0("cnn_predictions_", SATELLITE, "_", BANDS, ".Rds")))
  
}
