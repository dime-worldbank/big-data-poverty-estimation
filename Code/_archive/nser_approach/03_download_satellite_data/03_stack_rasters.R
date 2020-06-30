# Stack Rasters

# Google Earth Engine downloaded landsat bands as separate tifs

# Setup ------------------------------------------------------------------------
if(Sys.info()[["user"]] == "WB521633") project_file_path <- "C:/Users/wb521633/Dropbox/World Bank/IEs/Pakistan Poverty Estimation from Satellites"
if(Sys.info()[["user"]] == "robmarty") project_file_path <- "~/Dropbox/World Bank/IEs/Pakistan Poverty Estimation from Satellites"

library(stringr)
library(ggmap)
library(rgdal)
library(rgeos)
library(raster)

# Stack Rasters ----------------------------------------------------------------
unstacked_file_path <- file.path(project_file_path, "Data", "RawData", "Landsat", "unioncouncil", "unstacked")
unstacked_files <- list.files(unstacked_file_path)

unstacked_files_i = unstacked_files[500]
for(unstacked_files_i in unstacked_files){
  print(unstacked_files_i)
  
  b1 <- raster(file.path(unstacked_file_path, unstacked_files_i, "B1_median.tif"))
  b2 <- raster(file.path(unstacked_file_path, unstacked_files_i, "B2_median.tif"))
  b3 <- raster(file.path(unstacked_file_path, unstacked_files_i, "B3_median.tif"))
  b4 <- raster(file.path(unstacked_file_path, unstacked_files_i, "B4_median.tif"))
  b5 <- raster(file.path(unstacked_file_path, unstacked_files_i, "B5_median.tif"))
  b6 <- raster(file.path(unstacked_file_path, unstacked_files_i, "B6_median.tif"))
  b7 <- raster(file.path(unstacked_file_path, unstacked_files_i, "B7_median.tif"))
  r_stacked <- stack(b1, b2, b3, b4, b5, b6, b7)
  
  writeRaster(r_stacked, file.path(project_file_path, "Data", "RawData", "Landsat", "unioncouncil", "stacked", paste0(unstacked_files_i, ".tif")),overwrite=T)
  rm(r_stacked,b1, b2, b3, b4, b5, b6, b7)
}
