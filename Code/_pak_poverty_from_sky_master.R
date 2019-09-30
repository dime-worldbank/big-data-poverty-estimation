# Poverty Estimation from Satellite Imagery in Pakistan

# Filepaths --------------------------------------------------------------------
# Project file path
if(Sys.info()[["user"]] == "WB521633") project_file_path <- "C:/Users/wb521633/Dropbox/World Bank/IEs/Pakistan Poverty Estimation from Satellites"
if(Sys.info()[["user"]] == "robmarty") project_file_path <- "~/Dropbox/World Bank/IEs/Pakistan Poverty Estimation from Satellites"

rawdata_file_path <- file.path(project_file_path, "Data", "RawData")
final_data_file_path <- file.path(project_file_path,"Data", "FinalData")

# File path for NSER raw data (kept in separate folder than project file path for
# for data confidentialty). The raw data is only needed for: create_uc_level_survey_dataset.R. 
if(Sys.info()[["user"]] == "WB521633") nser_pitb_file_path <- "C:/Users/wb521633/Dropbox/NSER from PITB"
if(Sys.info()[["user"]] == "robmarty") nser_pitb_file_path <- "~/Dropbox/NSER from PITB"

# File path for BISP Geocodes
if(Sys.info()[["user"]] == "WB521633") bisp_geocodes_file_path <- "C:/Users/wb521633/WBG/Alice Duhaut - Pakistan" # ONEDRIVE FILE PATH
if(Sys.info()[["user"]] == "WB521633") bisp_rawdata_file_path <- "C:/Users/wb521633/WBG/Alice Duhaut - Pakistan/Data/RawData" # ONEDRIVE FILE PATH

# Packages ---------------------------------------------------------------------
library(rgdal)
library(readstata13)
library(dplyr)
library(data.table)
library(raster)
library(leaflet)
library(rgdal)
library(dplyr)
library(raster)
library(stringdist)
library(tmaptools)
library(stringr)
library(doBy)
library(rgeos)
library(haven)
library(velox)
library(sf)
library(tidyverse)

# Run Scripts ------------------------------------------------------------------
if(F){

# 1. Create Union Council Level Survey Dataset (csv) from Raw NSER data. 
# Averages variables within union councils and calculates number of observations 
# within each union council.
# NOTE: Need access to raw NSER data for this script; can skip if Union Council
#       level dataset is already created.
source(file.path(project_file_path, "Code", "create_uc_level_survey_dataset.R"))

# 2. Create Union Council Level Survey Shapefile 
# Merges union council level csv file with union council shapefile. Cleans 
# shapefile so that tehsil and union council names match with those from the
# survey data
source(file.path(project_file_path, "Code", "merge_survey_with_pak_boundary.R"))
}



