# Move data from Google Drive to Dropbox

SATELLITE <- "viirs"
UNDER_IA  <- "False"
BANDS     <- "rgb"

for(SATELLITE in c("viirs")){
  for(UNDER_IA in c("False")){
    for(BANDS in c("rgb", "ndvi", "bu")){
      
      PATH_NAME <- paste0("landsat_",SATELLITE,"_underia",UNDER_IA,"_b_",BANDS)
      
      print(paste(PATH_NAME, "-------------------------------------------------------"))
      
      # Features -------------------------------------------------------------------
      # In Google Colab, RAM maxed out trying to process full dataset at once, so 
      # outputted multiple datasets. Here we append them.
      cnn_features_df <- file.path(gdrive_dir,
                                   "Data",
                                   SURVEY_NAME,
                                   "FinalData",
                                   "cnn_features") %>%
        list.files(full.names = T) %>%
        str_subset(paste0("features_", PATH_NAME, "_")) %>%
        map_df(fread)
      
      cnn_features_df <- cnn_features_df %>% as.data.frame()
      cnn_features_df$uid <- cnn_features_df$uid %>%  str_replace_all("b'", "") %>% str_replace_all("'", "")
      
      cnn_features_df <- cnn_features_df %>%
        distinct(uid, .keep_all = T)
      
      saveRDS(cnn_features_df,
              file.path(data_dir,
                        SURVEY_NAME,
                        "FinalData",
                        "Individual Datasets",
                        "cnn_features",
                        paste0("cnn_features_", PATH_NAME, ".Rds")))
      
      # Predictions ----------------------------------------------------------------
      cnn_predictions_df <- fread(file.path(gdrive_dir,
                                            "Data",
                                            SURVEY_NAME,
                                            "FinalData",
                                            "cnn_predictions",
                                            paste0('predictions_', PATH_NAME,'.csv')))
      
      cnn_predictions_df$uid <- cnn_predictions_df$uid %>%  str_replace_all("b'", "") %>% str_replace_all("'", "")
      
      cnn_predictions_df <- cnn_predictions_df %>%
        distinct(uid, .keep_all = T)
      
      saveRDS(cnn_predictions_df %>% as.data.frame(),
              file.path(data_dir,
                        SURVEY_NAME,
                        "FinalData",
                        "Individual Datasets",
                        "cnn_features",
                        paste0("cnn_predictions_", PATH_NAME, ".Rds")))
      
    }
  }
}




