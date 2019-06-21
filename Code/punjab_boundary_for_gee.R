# Merge Survey Data with Pakistan Boundaries (Union Councils)
# Creates a shapefile of union councils with the survey data

# Setup ========================================================================
if(Sys.info()[["user"]] == "WB521633") project_file_path <- "C:/Users/wb521633/Dropbox/World Bank/IEs/Pakistan Poverty Estimation from Satellites"
if(Sys.info()[["user"]] == "robmarty") project_file_path <- "~/Dropbox/World Bank/IEs/Pakistan Poverty Estimation from Satellites"

library(rgdal)
library(readstata13)
library(dplyr)
library(raster)
library(stringdist)
library(tmaptools)
library(stringr)
library(doBy)
library(sf)

pak_sdf <- readOGR(dsn=file.path(project_file_path, "Data", "RawData", "Pakistan Boundaries", "OCHA"),
                  layer="Pak_adm1_pco_20110324")
pak_sdf$id <- 1
pak_sdf <- raster::aggregate(pak_sdf, by="id")

writeOGR(obj=pak_sdf,
         dsn=file.path(project_file_path,"Data","FinalData","Punjab for GEE"),
         driver="ESRI Shapefile",
         layer="punjab",
         overwrite_layer=T)

