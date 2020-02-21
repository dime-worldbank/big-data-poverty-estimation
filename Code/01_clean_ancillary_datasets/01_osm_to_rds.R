# Saves OSM as Rds Files

# OSM is downloaded as shapefiles. These take a long time to load. This script
# loads the shapefile and saves them as an Rds file, which is much quicker
# to load. 

files <- list.files(file.path(project_file_path, "Data", "RawData", "OSM"), pattern=".shp") %>% str_replace_all(".shp$", "")

for(file_i in files){
  print(file_i)
  data_sdf <- readOGR(dsn = file.path(project_file_path, "Data", "RawData", "OSM"), layer=file_i)
  saveRDS(file.path(project_file_path, "Data", "FinalData", "OSM", paste0(file_i, ".Rds")))
}


