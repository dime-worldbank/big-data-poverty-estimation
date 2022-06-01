# Saves OSM as Rds Files

# OSM is downloaded as shapefiles. These take a long time to load. This script
# loads the shapefile and saves them as an Rds file, which is much quicker
# to load. 

# Checks if file already created. If F, this doesn't process the file again.
OVERWRITE_FILES <- F

## Loop through country-year directories
country_year_dirs <- list.files(file.path(osm_dir, "RawData")) %>% 
  #str_replace_all(".shp", "") %>%
  str_subset("-free")

for(dir_i in country_year_dirs){
  print(paste(dir_i, "---------------------------------------------------------"))
  
  ## Create final Data Directory
  finaldata_dir <- file.path(osm_dir, "FinalData", dir_i) %>% 
    str_replace_all(".shp", "")
  dir.create(finaldata_dir)
  
  ## Loop through files; load raw data, save as Rds
  files <- list.files(file.path(osm_dir, "RawData", dir_i), pattern=".shp") %>%
    str_replace_all(".shp", "")
  
  ## Only use roads and pois
  files <- files %>% str_subset("pois|roads|points_of_interest")
  
  for(file_i in files){
    print(file_i)
    
    file_out_i <- file_i
    
    if(file_i %in% "hotosm_guf_points_of_interest_polygons") file_out_i <- "gis_osm_pois_a_free_1"
    if(file_i %in% "hotosm_guf_points_of_interest_points")   file_out_i <- "gis_osm_pois_free_1"
    if(file_i %in% "hotosm_guf_roads_lines")                 file_out_i <- "gis_osm_roads_free_1"
    
    OUT_PATH <- file.path(finaldata_dir, paste0(file_out_i, ".Rds"))
    
    if(!file.exists(OUT_PATH) | OVERWRITE_FILES){
      data_sdf <- readOGR(dsn = file.path(osm_dir, "RawData", dir_i), layer = file_i)
      
      ## Change Guiana to match other dataset formats
      # Guiana datasets are downloaded from hotosm, while all others are downloaded 
      # from geofabrik. geofabrik has an fclass variable which classifies the
      # feature class. Update Guiana to also have that variable.
      if((file_i %>% str_detect("hotosm_guf")) & 
         file_out_i %>% str_detect("pois")){
        
        data_sdf$fclass <- data_sdf$shop
        data_sdf$fclass[is.na(data_sdf$fclass)] <- data_sdf$amenity[is.na(data_sdf$fclass)]
        data_sdf$fclass[is.na(data_sdf$fclass)] <- data_sdf$tourism[is.na(data_sdf$fclass)]
        data_sdf$fclass[is.na(data_sdf$fclass)] <- data_sdf$man_made[is.na(data_sdf$fclass)]
        
      }
      
      if((file_i %>% str_detect("hotosm_guf")) & 
         file_out_i %>% str_detect("roads")){
        
        data_sdf@data <- data_sdf@data %>%
          dplyr::rename(fclass = highway)
        
      }
      
      saveRDS(data_sdf, OUT_PATH)
    }
    
  }
  
}



