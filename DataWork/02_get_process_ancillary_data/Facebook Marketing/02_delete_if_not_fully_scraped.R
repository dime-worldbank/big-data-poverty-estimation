# Check if all parameters scraped for a uid. May not have happened if there was 
# an error (eg, internet cut out). Delete these so will be rescraped

SURVEY_NAME = "DHS"

data <- file.path(data_dir, SURVEY_NAME,  "FinalData", "Individual Datasets",
                  "fb_mau_individual_datasets") %>%
  list.files(full.names = T, pattern = ".*Rds") %>%
  #str_subset("NG|GA") %>%
  map_df(readRDS)

data_sum <- data %>%
  group_by(uid) %>%
  dplyr::summarise(N = n()) 

uids_to_delete <- data_sum %>%
  dplyr::filter(N != 33) %>%
  pull(uid) 

if(length(uids_to_delete) > 0){
  
  uids_to_delete_rx <- uids_to_delete %>% paste(collapse = "|")
  
  files_to_delete <- file.path(data_dir, SURVEY_NAME,  "FinalData", "Individual Datasets",
                               "fb_mau_individual_datasets") %>%
    list.files(full.names = T, pattern = ".*Rds") %>%
    str_subset(uids_to_delete_rx)
  
  for(file_i in files_to_delete){
    file.remove(file_i)
  }
  
}



