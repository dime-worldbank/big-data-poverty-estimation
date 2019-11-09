# Stack Rasters

for(year in c(2013)){

  unstacked_file_path <- file.path(project_file_path, "Data", "RawData", "Landsat", "bisp_households", year, "unstacked")
  stacked_file_path <- file.path(project_file_path, "Data", "RawData", "Landsat", "bisp_households", year, "stacked")
  
  unstacked_files <- list.files(unstacked_file_path, include.dirs=T)
  
  for(folder in rev(unstacked_files)){
    print(paste0(folder, " --- ", year))
    
    b1 <- raster(file.path(unstacked_file_path, folder, "B1_median.tif"))
    b2 <- raster(file.path(unstacked_file_path, folder, "B2_median.tif"))
    b3 <- raster(file.path(unstacked_file_path, folder, "B3_median.tif"))
    b4 <- raster(file.path(unstacked_file_path, folder, "B4_median.tif"))
    b5 <- raster(file.path(unstacked_file_path, folder, "B5_median.tif"))
    b6 <- raster(file.path(unstacked_file_path, folder, "B6_median.tif"))
    b7 <- raster(file.path(unstacked_file_path, folder, "B7_median.tif"))
    r_stacked <- stack(b1, b2, b3, b4, b5, b6, b7)
    
    writeRaster(r_stacked, file.path(stacked_file_path, paste0(folder, ".tif")),overwrite=T)
    rm(r_stacked,b1, b2, b3, b4, b5, b6, b7)
  }
  
}





