# Append Annual BM Data

for(buffer_i in c(BUFFER_SATELLITE, 1120, 3360)){
  
  df <- file.path(data_dir, SURVEY_NAME, "FinalData", "Individual Datasets", 
                  "blackmarble") %>%
    list.files(full.names = T) %>%
    str_subset("bm_") %>%
    str_subset(as.character(buffer_i)) %>%
    map_df(readRDS)
  
  saveRDS(df, file.path(data_dir, SURVEY_NAME, "FinalData", "Individual Datasets", 
                        paste0("bm_", buffer_i, "m.Rds")))
}