# Stack Landsat Data

# First just try loading landsat to make sure folder isn't empty
unstacked_2011_file_path <- file.path(project_file_path, "Data", "RawData", "Landsat", "bisp_households", "2011", "unstacked")
unstacked_2013_file_path <- file.path(project_file_path, "Data", "RawData", "Landsat", "bisp_households", "2013", "unstacked")

stacked_2011_file_path <- file.path(project_file_path, "Data", "RawData", "Landsat", "bisp_households", "2011", "stacked")
stacked_2013_file_path <- file.path(project_file_path, "Data", "RawData", "Landsat", "bisp_households", "2013", "stacked")

# Stack 2011 Rasters -----------------------------------------------------------
unstacked_2011_files <- list.files(unstacked_2011_file_path)
unstacked_2011_files_i = unstacked_2011_files[1]
for(unstacked_2011_files_i in unstacked_2011_files){

  print(unstacked_2011_files_i)
  
  b1 <- raster(file.path(unstacked_2011_file_path, unstacked_2011_files_i, "B1_median.tif"))
  b2 <- raster(file.path(unstacked_2011_file_path, unstacked_2011_files_i, "B2_median.tif"))
  b3 <- raster(file.path(unstacked_2011_file_path, unstacked_2011_files_i, "B3_median.tif"))
  b4 <- raster(file.path(unstacked_2011_file_path, unstacked_2011_files_i, "B4_median.tif"))
  b5 <- raster(file.path(unstacked_2011_file_path, unstacked_2011_files_i, "B5_median.tif"))
  b6 <- raster(file.path(unstacked_2011_file_path, unstacked_2011_files_i, "B6_median.tif"))
  b7 <- raster(file.path(unstacked_2011_file_path, unstacked_2011_files_i, "B7_median.tif"))
  r_stacked <- stack(b1, b2, b3, b4, b5, b6, b7)
  
  writeRaster(r_stacked, file.path(stacked_2011_file_path, paste0(unstacked_2011_files_i, ".tif")),overwrite=T)
  rm(r_stacked,b1, b2, b3, b4, b5, b6, b7)
}

# Stack 2013 Rasters -----------------------------------------------------------
unstacked_2013_files <- list.files(unstacked_2013_file_path)
unstacked_2013_files <- unstacked_2013_files[!(unstacked_2013_files %in% c("1334", "1576", "5158", "5282"))]

unstacked_2013_files_i = unstacked_2013_files[1]
for(unstacked_2013_files_i in unstacked_2013_files){
  print(unstacked_2013_files_i)
  
  b1 <- raster(file.path(unstacked_2013_file_path, unstacked_2013_files_i, "B1_median.tif"))
  b2 <- raster(file.path(unstacked_2013_file_path, unstacked_2013_files_i, "B2_median.tif"))
  b3 <- raster(file.path(unstacked_2013_file_path, unstacked_2013_files_i, "B3_median.tif"))
  b4 <- raster(file.path(unstacked_2013_file_path, unstacked_2013_files_i, "B4_median.tif"))
  b5 <- raster(file.path(unstacked_2013_file_path, unstacked_2013_files_i, "B5_median.tif"))
  b6 <- raster(file.path(unstacked_2013_file_path, unstacked_2013_files_i, "B6_median.tif"))
  b7 <- raster(file.path(unstacked_2013_file_path, unstacked_2013_files_i, "B7_median.tif"))
  r_stacked <- stack(b1, b2, b3, b4, b5, b6, b7)
  
  writeRaster(r_stacked, file.path(stacked_2013_file_path, paste0(unstacked_2013_files_i, ".tif")),overwrite=T)
  rm(r_stacked,b1, b2, b3, b4, b5, b6, b7)
}







