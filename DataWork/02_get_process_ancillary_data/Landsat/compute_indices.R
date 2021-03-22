# Create Raster Tiles of Indices

band_combn <- combn(1:7, 2)

for(year in 2014){

  landsat_tiles <- list.files(file.path(project_file_path, "Data", "Landsat", "RawData", 2014)) %>% 
    str_replace_all(".tif|l8_2014_", "") %>%
    str_replace_all("_b*.", "") %>%
    str_replace_all("tile", "") %>%
    as.numeric() %>%
    unique()
  
  for(tile_i in landsat_tiles){
    for(combin_i in 1:ncol(band_combn)){
      
      paste("year", year,
            " - tile", tile_i,
            " - combin", combin_i) %>%
        print()
      
      band_combn_i <- band_combn[,combin_i]
      
      r1 <- raster(file.path(project_file_path, "Data", "Landsat", "RawData", year, 
                             paste0("l8_",year,"_tile",tile_i,"_b",band_combn_i[1],".tif"))) 
      r2 <- raster(file.path(project_file_path, "Data", "Landsat", "RawData", year, 
                             paste0("l8_",year,"_tile",tile_i,"_b",band_combn_i[2],".tif"))) 
      
      r_index <- overlay(r1,
                         r2,
                         fun=function(r1, r2){return((r2 - r1) / (r2 + r1))})
      
      writeRaster(r_index, 
                  file.path(project_file_path, "Data", "Landsat", "RawData", year, 
                            paste0("l8_",year,"_tile",tile_i,"_b",
                                   band_combn_i[1],"_", band_combn_i[2], "index",
                                   ".tif")))
      
    }
  }
}
