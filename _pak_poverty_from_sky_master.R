# Poverty Estimation from Satellite Imagery in Pakistan

# Root Directories -------------------------------------------------------------
#### Root Paths
# * Dropbox [dropbox_dir]: Where most data files are saved.
# * Google Drive [gdrive_dir]: We use Google Colab for processing ML models; here,
#   files for colab are saved in Google Drive
# * Secure [secure_dir]: Needed for PII data (World Bank OneDrive). Only needed
#   for OPM data
# * Github [github_dir]: Path to github repo
# * API Keys [api_key_dir]: Path where API keys are stored (api_keys.csv")

if(Sys.info()[["user"]] == "robmarty"){
  dropbox_dir <- "~/Dropbox/World Bank/IEs/Pakistan Poverty Estimation from Satellites"
  gdrive_dir <- "~/Google Drive/World Bank/IEs/Pakistan Poverty Estimation"
  secure_dir <- "~/Documents/World Bank/Pakistan Poverty from Sky" 
  github_dir <- "~/Documents/Github/Pakistan-Poverty-from-Sky"
  overleaf_dir <- "~/Dropbox/Apps/Overleaf/Poverty Estimation - Paper"
}

if(Sys.info()[["user"]] == "WB521633"){
  dropbox_dir <- "C:/Users/wb521633/Dropbox/World Bank/IEs/Pakistan Poverty Estimation from Satellites"
  #gdrive_dir
  secure_dir <- "C:/Users/wb521633/OneDrive - WBG/Pakistan Poverty from Sky - Survey Data" 
  github_dir <- "C:/Users/wb521633/Documents/GitHub/Pakistan-Poverty-from-Sky"
}

SURVEY_NAME <- "DHS"

# Paths from Root --------------------------------------------------------------

#### Dropbox Paths
data_dir         <- file.path(dropbox_dir, "Data")
opm_dir          <- file.path(data_dir, "OPM")
osm_dir          <- file.path(data_dir, "OSM")
dhs_dir          <- file.path(data_dir, "DHS")
gadm_dir         <- file.path(data_dir, "GADM")
fb_marketing_dir <- file.path(data_dir, "Facebook Marketing")
fb_rwi_dir       <- file.path(data_dir, "Facebook Relative Wealth Index")
globcover_dir    <- file.path(data_dir, "Globcover")

tables_dir  <- file.path(overleaf_dir, "tables")
figures_dir <- file.path(overleaf_dir, "figures")

api_key_dir <- file.path(dropbox_dir, "API Keys")

#### Google Drive Paths
gdrive_cnn_file_path <- file.path(gdrive_dir, "Data", "CNN")

# Parameters -------------------------------------------------------------------
PAK_UTM_PROJ <- "+init=epsg:24313"
PK_UTM_PROJ <- PAK_UTM_PROJ

define_country_proj <- function(country_code){
  UTM_PROJ <- NULL
  
  if(country_code %in% "BD") UTM_PROJ <- "+init=epsg:32646" # Bangladesh
  if(country_code %in% "IA") UTM_PROJ <- "+init=epsg:24347" # India
  if(country_code %in% "KH") UTM_PROJ <- "+init=epsg:3148" # Cambodia
  if(country_code %in% "KY") UTM_PROJ <- "+init=epsg:32644" # Kyrgyzstan
  if(country_code %in% "MM") UTM_PROJ <- "+init=epsg:23946" # Myanmar
  if(country_code %in% "NG") UTM_PROJ <- "+init=epsg:26332" # Nigeria
  if(country_code %in% "NP") UTM_PROJ <- "+init=epsg:32645" # Nepal
  if(country_code %in% "PH") UTM_PROJ <- "+init=epsg:32651" # Philippines
  if(country_code %in% "PK") UTM_PROJ <- "+init=epsg:24313" # Pakistan
  if(country_code %in% "SN") UTM_PROJ <- "+init=epsg:32628" # Senegal
  if(country_code %in% "TJ") UTM_PROJ <- "+init=epsg:32643" # Tajikistan
  if(country_code %in% "TL") UTM_PROJ <- "+init=epsg:32752" # Timor Leste
  if(country_code %in% "UG") UTM_PROJ <- "+init=epsg:21095" # Uganda
  
  return(UTM_PROJ)
}

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
library(spatialEco)
library(geosphere)
library(radiant.data)
library(readxl)
library(osmar)
library(tidyverse)
library(lubridate)
library(jsonlite)
library(httr)
library(curl)
library(haven)
library(httr)
library(mclust)
source(file.path(github_dir, "Functions", "functions.R"))

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



