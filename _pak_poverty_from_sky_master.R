# Poverty Estimation from Satellite Imagery in Pakistan

# Filepaths --------------------------------------------------------------------
# Project file path
if(Sys.info()[["user"]] == "WB521633") project_file_path <- "C:/Users/wb521633/Dropbox/World Bank/IEs/Pakistan Poverty Estimation from Satellites"
if(Sys.info()[["user"]] == "robmarty") project_file_path <- "~/Dropbox/World Bank/IEs/Pakistan Poverty Estimation from Satellites"

opm_dir <- file.path(project_file_path, "Data", "OPM")
osm_dir <- file.path(project_file_path, "Data", "OSM")
dhs_dir <- file.path(project_file_path, "Data", "DHS")
gadm_dir <- file.path(project_file_path, "Data", "GADM")
bisp_indiv_files_dir <- file.path(project_file_path, "Data", "BISP", "FinalData", "Individual Datasets")
data_dir <- file.path(project_file_path, "Data")
tables_file_path <- file.path(project_file_path, "Outputs", "Results", "Tables")
figures_file_path <- file.path(project_file_path, "Outputs", "Results", "Figures")

# Google Drive File Path
if(Sys.info()[["user"]] == "robmarty") gdrive_file_path <- "~/Google Drive/World Bank/IEs/Pakistan Poverty Estimation"

gdrive_cnn_file_path <- file.path(gdrive_file_path, "Data", "CNN")

# Secure Directory
if(Sys.info()[["user"]] == "WB521633") secure_file_path <- "C:/Users/wb521633/OneDrive - WBG/Pakistan Poverty from Sky - Survey Data" 
if(Sys.info()[["user"]] == "robmarty") secure_file_path <- "~/Documents/World Bank/Pakistan Poverty from Sky"

# Code File Path
if(Sys.info()[["user"]] == "WB521633") code_file_path <- "C:/Users/wb521633/OneDrive - WBG/Documents/GitHub/Pakistan-Poverty-from-Sky"
if(Sys.info()[["user"]] == "robmarty") code_file_path <- "~/Documents/Github/Pakistan-Poverty-from-Sky"

# Webscraping File Path
if(Sys.info()[["user"]] == "WB521633") webscraping_api_filepath <- "C:/Users/wb521633/Dropbox/World Bank/Webscraping/Files for Server"
if(Sys.info()[["user"]] == "robmarty") webscraping_api_filepath <- "~/Dropbox/World Bank/Webscraping/Files for Server"

# Parameters -------------------------------------------------------------------
PAK_UTM_PROJ <- "+init=epsg:24313"

# Packages ---------------------------------------------------------------------
library(rgdal)
library(viridis)
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
library(alluvial)
library(ggmap)
library(velox)
library(sf)
library(sp)
library(raster)
library(rgeos)
library(tidyverse)
library(caret)
library(mltest)
library(RANN)
library(tidyverse)
library(lubridate)
library(jsonlite)
library(httr)
library(curl)
library(ggpmisc)
library(haven)
library(sjmisc)
library(dbscan)
library(ggplot2)
library(geosphere)
library(radiant.data)
library(osmar)
source(file.path(code_file_path, "Functions", "functions.R"))

source("https://raw.githubusercontent.com/ramarty/fast-functions/master/R/functions_in_chunks.R")

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



