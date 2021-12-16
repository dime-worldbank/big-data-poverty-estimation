# Append satellite data extracted from GEE

# Previous script extracts data from GEE; values are aggregated at each survey
# location (eg, average nighttime lights near a survey location). There are
# memory issues trying to process everything at the same time; consequently, 
# we break up into multiple files. Files from GEE are saved into Google Drive.
# Here, we (1) load and append files from GEE and (2) save into dropbox

# Delete data from both google drive and dropbox if not all data extracted
# from GEE. Here, we check number of rows. From GEE, there should be an 
# observation for each row. Not having all the rows indicates there was an issue
# extracting data from GEE. When extracting data from GEE, we can only re-extract
# data files that have not been fully downloaded; consequently, deleting here
# will ensure GEE tries to reextract.  
DELETE_IF_NOT_ALL_DATA <- F

# Data directory and root data names -------------------------------------------
## Filepath to GEE directory
GEE_PATH <- file.path(gdrive_dir, 
                      "Data", 
                      SURVEY_NAME, 
                      "FinalData", 
                      "Individual Datasets", 
                      paste0("satellite_data_from_gee_", tolower(SURVEY_NAME)))

OUT_PATH <- file.path(data_dir,
                      SURVEY_NAME,
                      "FinalData",
                      "Individual Datasets", 
                      "satellite_data_from_gee")

## Remove on gdrive folder if has a parenthesis in the name -- eg (1).csv. 
## These are duplicates.
files_to_rm <- list.files(GEE_PATH) %>%
  str_subset("\\(")

length(files_to_rm)
for(file_i in files_to_rm){
  file.remove(file.path(GEE_PATH, file_i))
}

## Root data names
# Exclude [year]_[chunk] -- so unique per each dataframe 
data_root_names <- list.files(GEE_PATH) %>% 
  str_replace_all("_[:digit:]{4}_[:digit:]{1,2}.csv", "") %>%
  str_replace_all("gee_", "") %>%
  unique() %>%
  sort()

# Load and append --------------------------------------------------------------
# Check how many observations each data file has
df_nrow <- data.frame(NULL)

for(data_root_names_i in data_root_names){
  print(data_root_names_i)
  
  df_tmp <- GEE_PATH %>%
    list.files(full.names = T) %>%
    str_subset(data_root_names_i) %>%
    map_df(read.csv) %>%
    mutate_if(is.factor, as.character) %>%
    distinct()
    #distinct(uid, year, .keep_all = T) # FIX SO DONT NEED THIS
  
  saveRDS(df_tmp, file.path(OUT_PATH, paste0(data_root_names_i, ".Rds")))
  
  df_nrow <- bind_rows(
    df_nrow,
    data.frame(N_rows = nrow(df_tmp),
               name = data_root_names_i)
  )
}

# Delete if don't have all the data --------------------------------------------
if(DELETE_IF_NOT_ALL_DATA){
  
  survey_df <- readRDS(file.path(data_dir, SURVEY_NAME, "FinalData", 
                                 "Individual Datasets", "survey_socioeconomic.Rds"))
  
  data_to_rm <- df_nrow$name[df_nrow$N_rows != nrow(survey_df)] %>%
    paste(collapse = "|")
  
  print("Removing...")
  print(data_to_rm)
  
  gee_to_rm <- GEE_PATH %>%
    list.files(full.names = T) %>%
    str_subset(data_to_rm)
  
  db_to_rm <- OUT_PATH %>%
    list.files(full.names = T) %>%
    str_subset(data_to_rm)
  
  for(file_i in c(gee_to_rm, db_to_rm)){
    file.remove(file_i)
  }
  
}













