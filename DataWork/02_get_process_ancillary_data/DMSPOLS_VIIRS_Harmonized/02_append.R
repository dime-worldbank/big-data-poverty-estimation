# Append Harmonized NTL Data

## Load data
for(buffer_i in c(BUFFER_SATELLITE, 1120, 3360)){
  
  ntl_all_df <- file.path(data_dir, SURVEY_NAME, "FinalData", "Individual Datasets", "ntl_harmonized") %>%
    list.files(pattern = "ntlharmon_", full.names = T) %>%
    str_subset(paste0(buffer_i,"m")) %>%
    map_df(readRDS)
  
  ## Export data
  saveRDS(ntl_all_df, file.path(data_dir, SURVEY_NAME, "FinalData", "Individual Datasets", 
                                paste0("ntl_harmonized_",buffer_i,".Rds")))
  
}



