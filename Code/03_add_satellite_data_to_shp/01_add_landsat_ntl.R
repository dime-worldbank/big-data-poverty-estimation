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

# Load Shapefile ---------------------------------------------------------------
uc_sdf <- st_read(file.path(project_file_path, "Data", "FinalData", "UC with NSER Data", "uc_nser.geojson")) %>% as("Spatial")
uc_sdf$uc_id <- as.character(uc_sdf$uc_id)

# Extract Satellite Imagery to Shapefile ---------------------------------------
#### DMSP-OLS
for(year in 1992:2013){
  print(year)
  dmspols <- raster(file.path(project_file_path, "Data", "RawData", "DMSPOLS", paste0("pak_dmspols_",year,".tif")))
  uc_sdf[[paste0("dmspols_",year,"_mean")]] <- velox(dmspols)$extract(uc_sdf, function(x) mean(x, na.rm=T))
}

#### VIIRS
for(year in 2012:2018){
  print(year)
  viirs <- raster(file.path(project_file_path, "Data", "RawData", "VIIRS", "VIIRS Annual", paste0("pak_viirs_median_",year,".tif")))
  uc_sdf[[paste0("viirs_",year,"_mean")]] <- velox(viirs)$extract(uc_sdf, function(x) mean(x, na.rm=T))
}

#### Landsat
landsat_average <- lapply(uc_sdf$uc_id, function(id){
  print(id)
  r <- brick(file.path(stacked_file_path, paste0(id,".tif")))
  #landsat_band_mean <- cellStats(r, stat='mean') * c(0.0001, 0.0001, 0.0001, 0.0001, 0.0001, 0.1, 0.0001) / 1000
  landsat_band_mean <- cellStats(r, stat='mean') * 0.0001 / 1000
  landsat_band_mean <- as.data.frame(t(landsat_band_mean))
  names(landsat_band_mean) <- paste0("landsat2010_band_",1:7,"_mean")
  landsat_band_mean$uc_id <- id
  return(landsat_band_mean)
}) %>%
  bind_rows

uc_sdf <- merge(uc_sdf, landsat_average, by="uc_id", all.x=T)
uc_sf <- st_as_sf(uc_sdf)

# Export -----------------------------------------------------------------------
st_write(uc_sf, file.path(project_file_path, "Data", "FinalData", "UC with NSER Data", "uc_nser_satelliteaverages.geojson"),delete_dsn=T)



