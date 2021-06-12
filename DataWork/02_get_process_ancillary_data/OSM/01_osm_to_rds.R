# Saves OSM as Rds Files

# OSM is downloaded as shapefiles. These take a long time to load. This script
# loads the shapefile and saves them as an Rds file, which is much quicker
# to load. 

for(year in 2014:2020){
  print(year)
  
  year_file <- paste0("pakistan-",year,"0101-free.shp")
  year_file_out <- year_file %>% str_replace_all(".shp", "")
  
  files <- list.files(file.path(project_file_path, "Data", "OSM", "RawData", year_file), pattern=".shp") %>% str_replace_all(".shp", "")
  
  for(file_i in files){
    print(file_i)
    data_sdf <- readOGR(dsn = file.path(project_file_path, "Data", "OSM", "RawData", year_file), layer = file_i)
    saveRDS(data_sdf, file.path(project_file_path, "Data", "OSM", "FinalData", year_file_out, paste0(file_i, ".Rds")))
  }
}


