# Saves OSM as Rds Files

# OSM is downloaded as shapefiles. These take a long time to load. This script
# loads the shapefile and saves them as an Rds file, which is much quicker
# to load. 

# Checks if file already created. If F, this doesn't process the file again.
OVERWRITE_FILES <- F

## Loop through country-year directories
country_year_dirs <- list.files(file.path(project_file_path, "Data", "OSM", "RawData"), pattern=".shp") 

country_year_dirs <- country_year_dirs %>% str_subset("nepal")

for(dir_i in country_year_dirs){
  print(paste(dir_i, "---------------------------------------------------------"))
  
  ## Create final Data Directory
  finaldata_dir <- file.path(project_file_path, "Data", "OSM", "FinalData", dir_i %>% str_replace_all(".shp", ""))
  dir.create(finaldata_dir)
  
  ## Loop through files; load raw data, save as Rds
  files <- list.files(file.path(project_file_path, "Data", "OSM", "RawData", dir_i), pattern=".shp") %>% str_replace_all(".shp", "")
  
  for(file_i in files){
    print(file_i)
    
    OUT_PATH <- file.path(finaldata_dir, paste0(file_i, ".Rds"))
    
    if(!file.exists(OUT_PATH) | OVERWRITE_FILES){
      data_sdf <- readOGR(dsn = file.path(project_file_path, "Data", "OSM", "RawData", dir_i), layer = file_i)
      saveRDS(data_sdf, OUT_PATH)
    }
    
  }
  
}



