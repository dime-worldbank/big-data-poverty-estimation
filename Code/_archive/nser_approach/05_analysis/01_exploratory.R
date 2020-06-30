# Merge Rasters

# Setup ------------------------------------------------------------------------
if(Sys.info()[["user"]] == "WB521633") project_file_path <- "C:/Users/wb521633/Dropbox/World Bank/IEs/Pakistan Poverty Estimation from Satellites"
if(Sys.info()[["user"]] == "robmarty") project_file_path <- "~/Dropbox/World Bank/IEs/Pakistan Poverty Estimation from Satellites"

stacked_file_path <- file.path(project_file_path, "Data", "RawData", "Landsat", "unioncouncil", "stacked")

library(stringr)
library(ggmap)
library(rgdal)
library(rgeos)
library(raster)
library(sf)
library(dplyr)
library(velox)
library(keras)

# EXAMPLE
boston_housing <- dataset_boston_housing()

c(train_data, train_labels) %<-% boston_housing$train
c(test_data, test_labels) %<-% boston_housing$test

# Load Shapefile ---------------------------------------------------------------
shp <- st_read(file.path(project_file_path, "Data", "FinalData", "UC with NSER Data", "uc_nser_satelliteaverages.geojson")) %>% as("Spatial")

train_y <- shp$asset_item_index_mean
train_x <- subset(shp@data, select=c(viirs_2012_mean,
                                landsat2010_band_1_mean, 
                                landsat2010_band_2_mean,
                                landsat2010_band_3_mean,
                                landsat2010_band_4_mean,
                                landsat2010_band_5_mean,
                                landsat2010_band_6_mean,
                                landsat2010_band_7_mean))

model <- keras_model_sequential() %>%
  layer_dense(units = 64, activation = "relu", 
              input_shape = dim(train_x)[2]) %>%
  layer_dense(units = 64, activation = "relu") %>%
  layer_dense(units = 1)


head(shp)

shp$ndvi <- (shp$landsat2010_band_4_mean - shp$landsat2010_band_3_mean) / (shp$landsat2010_band_4_mean + shp$landsat2010_band_3_mean)

lm(asset_item_index_mean ~ dmspols_2010_mean, data=shp) %>% summary
lm(asset_item_index_mean ~ landsat2010_band_1_mean + landsat2010_band_2_mean + landsat2010_band_3_mean + landsat2010_band_4_mean + landsat2010_band_5_mean + landsat2010_band_6_mean + landsat2010_band_7_mean, data=shp) %>% summary
lm(asset_item_index_mean ~ dmspols_2010_mean + landsat2010_band_1_mean + landsat2010_band_2_mean + landsat2010_band_3_mean + landsat2010_band_4_mean + landsat2010_band_5_mean + landsat2010_band_6_mean + landsat2010_band_7_mean, data=shp) %>% summary

lm(asset_item_index_mean ~ ndvi, data=shp) %>% summary

lm(asset_item_index_mean ~ viirs_2012_mean, data=shp) %>% summary


