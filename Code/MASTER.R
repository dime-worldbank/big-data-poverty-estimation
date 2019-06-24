# Poverty Estimation from Satellite Imagery in Pakistan

# Filepaths --------------------------------------------------------------------
# Project file path
if(Sys.info()[["user"]] == "WB521633") project_file_path <- "C:/Users/wb521633/Dropbox/World Bank/IEs/Pakistan Poverty Estimation from Satellites"
if(Sys.info()[["user"]] == "robmarty") project_file_path <- "~/Dropbox/World Bank/IEs/Pakistan Poverty Estimation from Satellites"

# File path for NSER raw data (kept in separate folder than project file path for
# for data confidentialty). The raw data is only needed for: create_uc_level_survey_dataset.R. 
if(Sys.info()[["user"]] == "WB521633") nser_pitb_file_path <- "C:/Users/wb521633/Dropbox/NSER from PITB"
if(Sys.info()[["user"]] == "robmarty") nser_pitb_file_path <- "~/Dropbox/NSER from PITB"

# Packages ---------------------------------------------------------------------
library(rgdal)
library(readstata13)
library(dplyr)
library(data.table)
library(raster)
library(leaflet)
library(rgdal)
library(readstata13)
library(dplyr)
library(raster)
library(stringdist)
library(tmaptools)
library(stringr)
library(doBy)
library(sf)



