# Poverty Estimation from Satellite Imagery in Pakistan

# DHS
# PAK_POINTS
# PAK_CITY_POINTS
# DHS_policy_experiment
# DHS_all_policy_experiment
# DHS_nga_policy_experiment
# DHS
# LSMS
SURVEY_NAME <- "DHS"

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
  github_dir <- "~/Documents/Github/big-data-poverty-estimation"
  overleaf_global_dir <- "~/Dropbox/Apps/Overleaf/Poverty Estimation - Global Paper"
  overleaf_pak_dir <- "~/Dropbox/Apps/Overleaf/Poverty Estimation - Pakistan Paper"
}

if(Sys.info()[["user"]] == "WB521633"){
  dropbox_dir <- "C:/Users/wb521633/Dropbox/World Bank/IEs/Pakistan Poverty Estimation from Satellites"
  #gdrive_dir
  secure_dir <- "C:/Users/wb521633/OneDrive - WBG/Pakistan Poverty from Sky - Survey Data" 
  github_dir <- "C:/Users/wb521633/Documents/GitHub/Pakistan-Poverty-from-Sky"
}

# Paths from Root --------------------------------------------------------------

#### Dropbox Paths
data_dir         <- file.path(dropbox_dir, "Data")
opm_dir          <- file.path(data_dir, "OPM")
osm_dir          <- file.path(data_dir, "OSM")
dhs_dir          <- file.path(data_dir, "DHS")
lsms_dir         <- file.path(data_dir, "LSMS")
dhs_exp_dir      <- file.path(data_dir, "DHS_policy_experiment")
dhs_nga_exp_dir  <- file.path(data_dir, "DHS_nga_policy_experiment")
dhs_all_exp_dir  <- file.path(data_dir, "DHS_all_policy_experiment")
gadm_dir         <- file.path(data_dir, "GADM")
ntl_harmon_dir   <- file.path(data_dir, "DMSPOLS_VIIRS_Harmonized")
ntl_bm_dir       <- file.path(data_dir, "NTL Black Marble")
fb_marketing_dir <- file.path(data_dir, "Facebook Marketing")
fb_rwi_dir       <- file.path(data_dir, "Facebook Relative Wealth Index")
globcover_dir    <- file.path(data_dir, "Globcover")
worldclim_dir    <- file.path(data_dir, "WorldClim")
cntry_dtls_dir   <- file.path(data_dir, "Country Details")
sentinel5p_dir   <- file.path(data_dir, "Sentinel 5P Pollution")

#### Overleaf Paths
tables_global_dir  <- file.path(overleaf_global_dir, "tables")
figures_global_dir <- file.path(overleaf_global_dir, "figures")
stats_global_dir   <- file.path(overleaf_global_dir, "stats")

tables_pak_dir  <- file.path(overleaf_pak_dir, "tables")
figures_pak_dir <- file.path(overleaf_pak_dir, "figures")

#### API Key Paths (For Facebook)
api_key_dir <- file.path(dropbox_dir, "API Keys")

#### Google Drive Paths
gdrive_cnn_file_path <- file.path(gdrive_dir, "Data", "CNN")

# Create Directory Structure for Survey Data -----------------------------------
survey_name_i <- "DHS"

for(survey_name_i in c("DHS", "LSMS", "DHS_all_policy_experiment",  "DHS_policy_experiment", "DHS_nga_policy_experiment", "OPM", "OPM_GPSDISP_DHS", "PAK_POINTS", "PAK_CITY_POINTS", "LAGOS_POINTS")){
  
  ### DROPBOX
  file.path(data_dir, survey_name_i) %>% dir.create()
  file.path(data_dir, survey_name_i, "FinalData") %>% dir.create()
  
  # FinalData/
  file.path(data_dir, survey_name_i, "FinalData", "Individual Datasets") %>% dir.create()
  file.path(data_dir, survey_name_i, "FinalData", "Merged Datasets") %>% dir.create()
  file.path(data_dir, survey_name_i, "FinalData", "pov_estimation_results") %>% dir.create()
  
  # FinalData/Individual Datasets
  file.path(data_dir, survey_name_i, "FinalData", "Individual Datasets", "fb_mau_individual_datasets") %>% dir.create()
  file.path(data_dir, survey_name_i, "FinalData", "Individual Datasets", "globcover") %>% dir.create()
  file.path(data_dir, survey_name_i, "FinalData", "Individual Datasets", "osm") %>% dir.create()
  file.path(data_dir, survey_name_i, "FinalData", "Individual Datasets", "satellite_data_from_gee") %>% dir.create()
  file.path(data_dir, survey_name_i, "FinalData", "Individual Datasets", "worldclim") %>% dir.create()
  file.path(data_dir, survey_name_i, "FinalData", "Individual Datasets", "cnn_features") %>% dir.create()
  file.path(data_dir, survey_name_i, "FinalData", "Individual Datasets", "ntl_harmonized") %>% dir.create()
  file.path(data_dir, survey_name_i, "FinalData", "Individual Datasets", "blackmarble") %>% dir.create()
  
  # FinalData/Individual Datasets/osm
  file.path(data_dir, survey_name_i, "FinalData", "Individual Datasets", "osm", "poi") %>% dir.create()
  file.path(data_dir, survey_name_i, "FinalData", "Individual Datasets", "osm", "roads_density") %>% dir.create()
  file.path(data_dir, survey_name_i, "FinalData", "Individual Datasets", "osm", "roads_distance") %>% dir.create()
  
  file.path(data_dir, survey_name_i, "FinalData", "pov_estimation_results", "predictions") %>% dir.create()
  file.path(data_dir, survey_name_i, "FinalData", "pov_estimation_results", "feature_importance") %>% dir.create()
  file.path(data_dir, survey_name_i, "FinalData", "pov_estimation_results", "accuracy") %>% dir.create()
  file.path(data_dir, survey_name_i, "FinalData", "pov_estimation_results", "grid_search") %>% dir.create()
  
  file.path(data_dir, survey_name_i, "FinalData", "cnn_features", "split_into_data_subsets") %>% dir.create()
  
  ### GOOGLE DRIVE
  file.path(gdrive_dir, "Data", survey_name_i) %>% dir.create()
  file.path(gdrive_dir, "Data", survey_name_i, "FinalData") %>% dir.create()
  
  # FinalData/
  file.path(gdrive_dir, "Data", survey_name_i, "FinalData", "cnn_features") %>% dir.create()
  file.path(gdrive_dir, "Data", survey_name_i, "FinalData", "cnn_models") %>% dir.create()
  file.path(gdrive_dir, "Data", survey_name_i, "FinalData", "cnn_predictions") %>% dir.create()
  file.path(gdrive_dir, "Data", survey_name_i, "FinalData", "Individual Datasets") %>% dir.create()
  
  file.path(gdrive_dir, "Data", survey_name_i, "FinalData", "cnn_features", "split_into_data_subsets") %>% dir.create()
  
  # FinalData/Individual Datasets/
  file.path(gdrive_dir, "Data", survey_name_i, "FinalData", "Individual Datasets", 
            paste0("satellite_data_from_gee_", tolower(survey_name_i))) %>% dir.create()
  
  file.path(gdrive_dir, "Data", survey_name_i, "FinalData", "Individual Datasets", "cnn_l8") %>% dir.create()
  file.path(gdrive_dir, "Data", survey_name_i, "FinalData", "Individual Datasets", "cnn_l8", "tfrecords") %>% dir.create()
}

# Parameters -------------------------------------------------------------------
PAK_UTM_PROJ <- "+init=epsg:24313"
PK_UTM_PROJ <- PAK_UTM_PROJ

#### Buffers
if(SURVEY_NAME %>% str_detect("DHS")){
  BUFFER_OSM       <- 5000
  BUFFER_SATELLITE <- 2500
} 

if(SURVEY_NAME %>% str_detect("LSMS")){
  BUFFER_OSM       <- 5000
  BUFFER_SATELLITE <- 2500
} 

if(SURVEY_NAME %in% "PAK_POINTS"){
  BUFFER_OSM       <- 1500
  BUFFER_SATELLITE <- 1500
} 

if(SURVEY_NAME %in% "PAK_CITY_POINTS"){
  BUFFER_OSM       <- 750
  BUFFER_SATELLITE <- 750
} 

if(SURVEY_NAME %in% "LAGOS_POINTS"){
  BUFFER_OSM       <- 1000
  BUFFER_SATELLITE <- 1000
} 

# Packages ---------------------------------------------------------------------
library(rgdal)
library(viridis)
library(readstata13)
library(dplyr)
library(data.table)
library(raster)
library(leaflet)
library(stargazer)
library(rgdal)
library(dplyr)
library(raster)
library(stringdist)
library(tmaptools)
library(stringr)
library(doBy)
library(geosphere)
library(rgeos)
library(haven)
library(alluvial)
library(ggmap)
library(velox)
library(sf)
library(sp)
library(glmnet)
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
library(missMDA)
library(DescTools)
library(FactoMineR)
library(countrycode)
library(furrr)
library(progressr)
library(ggmap)
library(ggridges)
library(ggpubr)
library(xgboost)
library(WDI)
library(ggtext)
library(gghalves)
library(ggthemes)
library(rnaturalearth)
library(ggrepel)
library(ggcorrplot)
library(scales)
library(ggExtra)
library(gghalves)
library(exactextractr)
library(ggsignif)
library(LiblineaR)
library(caret)
source(file.path(github_dir, "Functions", "functions.R"))

source("https://raw.githubusercontent.com/ramarty/download_blackmarble/main/R/download_blackmarble.R")

source("https://raw.githubusercontent.com/ramarty/fast-functions/master/R/functions_in_chunks.R")

source("https://raw.githubusercontent.com/ramarty/rSocialWatcher/52eede6cf561a74584503846eb78ee8bc8fa780b/R/main.R")

#source("https://raw.githubusercontent.com/ramarty/rSocialWatcher/main/R/fill_fb_id_names.R")
#source("https://raw.githubusercontent.com/ramarty/rSocialWatcher/main/R/get_fb_parameter_ids.R")
#source("https://raw.githubusercontent.com/ramarty/rSocialWatcher/main/R/get_fb_suggested_radius.R")
#source("https://raw.githubusercontent.com/ramarty/rSocialWatcher/main/R/query_fb_marketing_api.R")

# Define functions -------------------------------------------------------------

coef_of_det <- function(y, x){
  summary(lm(y ~ x))$r.squared
}

# Run Scripts ------------------------------------------------------------------

#### PARAMETERS
# Whether to re-run code that creates the dataset with Facebook parameters. This
# requires and API key, and may not work if Facebook updated the version of the
# API. For example, may get can error that says need to update the API to the 
# latest version. If this happens, need to go to the Facebook developer 
# platform and changed the API to the latest version.
RERUN_FB_CREATE_PARAM_DATASET <- F

# Whether to re-process the preparation of OSM files. This includes:
# (1) Converting from shp to Rds;
# (2) Splitting India by ADM2 region
# (3) Splitting OSM files into unique countries.
# These processes take a long time.
PREP_OSM_FILES <- F

# When extracting ancillary data to survey locations, whether to skip files that
# are already extracted. For example, if globcover data already extracted for
# Kenya, skip extracting for Kenya; only implement for countries+datasets where
# need to extract data.
REPLACE_IF_EXTRACTED <- F

#### FILE PATHS FOR CODE
datawork_dir <- file.path(github_dir, "DataWork")

anc_dir              <- file.path(datawork_dir, "02_get_process_ancillary_data")
anc_globcover_dir    <- file.path(anc_dir, "Globcover")
anc_worldclim_dir    <- file.path(anc_dir, "WorldClim")
anc_fb_marketing_dir <- file.path(anc_dir, "Facebook Marketing")
anc_fb_rwi_dir       <- file.path(anc_dir, "Facebook Relative Wealth Index")
anc_osm_dir          <- file.path(anc_dir, "OSM")
anc_satellite_dir    <- file.path(anc_dir, "Satellite Data")
anc_cnn_features_dir <- file.path(anc_dir, "CNN Features Predict NTL")
anc_s5p_dir          <- file.path(anc_dir, "Sentinel 5P Pollution")
anc_dmspharmon_dir   <- file.path(anc_dir, "DMSPOLS_VIIRS_HARMONIZED")

#### RUN CODE
if(F){
  # 0. Download GADM -----------------------------------------------------------
  # We use GADM across multiple files, so we download.
  # -- 01_download_gadm.R: downloads GADM up to ADM2.
  # -- 02_clean_adm2.R: cleans the data. Some countries don't have ADM2; in these
  #    cases, to standardize the data, we assign the ADM2 name to just be the
  #    ADM1 name.
  source(file.path(datawork_dir, "00_download_gadm", "01_download_gadm.R"))
  source(file.path(datawork_dir, "00_download_gadm", "02_clean_adm2.R"))
  
  # 1. Clean DHS Data ----------------------------------------------------------
  # Cleans DHS data. Appends DHS data across countries, prepares poverty 
  # variables (eg, asset indices) and aggregates to cluster level.
  # -- 01_clean_dhs.R: Appends data across countries
  # -- 02_clean_dhs_varconstruction.R: Consructs variables and aggregates to 
  #    cluster level.
  source(file.path(datawork_dir, "01_clean_dhs", "01_clean_dhs.R"))
  source(file.path(datawork_dir, "01_clean_dhs", "02_clean_dhs_varconstruction.R"))
  source(file.path(datawork_dir, "01_clean_dhs", "03_make_within_country_folds.R"))
  source(file.path(datawork_dir, "01_clean_dhs", "04_merge_data_with_folds.R"))
  
  # 2. Get/Process Ancillary Data ----------------------------------------------
  # Extract variables to survey locations (eg, satellite data, facebook data, etc)
  
  # ** 2.1 Globcover -----------------------------------------------------------
  # Extract globcover data; proportion of area near survey classified according
  # to each class in globcover.
  # -- 01_extract_globcover.R: Extracts data; saves data for each country
  # -- 02_append.R Appends data for each country.
  source(file.path(anc_globcover_dir, "01_extract_globcover.R"))
  source(file.path(anc_globcover_dir, "02_append.R"))
  
  # ** 2.1 WorldClim -----------------------------------------------------------
  # Extract data from WorldClim (eg, temperature and precipitation)
  # -- 01_extract_worldclim.R: Extracts data; saves data for each country
  # -- 02_append.R Appends data for each country.
  source(file.path(anc_worldclim_dir, "01_extract_worldclim.R"))
  source(file.path(anc_worldclim_dir, "02_append.R"))
  
  # ** 2.1 OSM -----------------------------------------------------------------
  # Extracts data from OSM. Uses the points of interest (POI) and roads dataset
  # to detemine (1) number of POI near survey locations, (2) distance to nearest
  # POI (by type), (3) road density by road type and (4) distance to nearest
  # road (by type)
  # -- 01_osm_to_rds.R: OSM originally downloaded as shapefiles. These take
  #    forever to load. Here, we load and save as .Rds files, which are faster
  #    to load.
  # -- 02_split_india_by_gadm.R: India has very large OSM files, which makes
  #    processing difficult. Here, we break up files so that there's a file for
  #    each ADM2 within India. When masking by ADM2, we buffer the ADM2 first.
  #    When processing the data later, we first check which ADM2 the survey is 
  #    in the grab the relevant ADM2 OSM file for India.
  # -- 02_split_to_unique_countries.R: Files are downloaded from http://www.geofabrik.de/,
  #    which typically has data prepped for each country. Occasinally, they
  #    combine countries into one dataset (eg, Haiti and Dominican Rep. in one
  #    datast as opposed to two). Here, we split datasets so that there's one
  #    dataset per country.
  # -- 03_extract_poi.R: Extracts density and distance from POI dataset.
  # -- 03_extract_road_density.R: Extracts road density near each survey location.
  # -- 03_extract_road_distance.R: Extracts road distance near each survey location.
  # -- 04_merge_data.R: Merges all OSM data together. Above, data is saved for
  #    each country. This script appends those files and merges the POI and road
  #    files together.
  if(PREP_OSM_FILES){
    source(file.path(anc_osm_dir, "01_osm_to_rds.R"))
    source(file.path(anc_osm_dir, "02_split_india_by_gadm.R"))
    source(file.path(anc_osm_dir, "02_split_to_unique_countries.R"))
  }
  source(file.path(anc_osm_dir, "03_extract_poi.R"))
  source(file.path(anc_osm_dir, "03_extract_road_density.R"))
  source(file.path(anc_osm_dir, "03_extract_road_distance.R"))
  #source(file.path(anc_osm_dir, "04_merge_data.R"))
  source(file.path(anc_osm_dir, "04_append_clean_poi.R"))
  source(file.path(anc_osm_dir, "04_append_clean_road.R"))
  
  # ** 2.1 Facebook Marketing Data ---------------------------------------------
  # Extracts Facebook Marketing data around each survey location (monthly and
  # daily active users across a number of characteristics).
  # -- 01_search_behavior_interests_IDs.R: Creates dataset of behaviors and
  #    interests and associated IDs. This dataset helps construct the API
  #    requests for scraping data.
  # -- 02_scrape_facebook_data.R: Scrapes Facebook Data; creates a dataset
  #    for each survey location.
  # -- 03_append_data.R: Appends the above data together. Also saves a dataset
  #    of the parameters used when scraping data 
  # -- 04_clean_param_df.R: Cleans parameter dataframe, one of the datasets
  #    created above. Makes interpretable categories; for example, instead of
  #    indicating "scraped education levels 1,2,3,4...", say "up to high school"
  if(RERUN_FB_CREATE_PARAM_DATASET){
    source(file.path(anc_fb_marketing_dir, "01_search_behavior_interests_IDs.R"))
  }
  source(file.path(anc_fb_marketing_dir, "04a_scrape_fb.R"))
  source(file.path(anc_fb_marketing_dir, "04b_append_data.R"))
  #source(file.path(anc_fb_marketing_dir, "02_scrape_facebook_data.R"))
  #source(file.path(anc_fb_marketing_dir, "03_append_data.R"))
  #source(file.path(anc_fb_marketing_dir, "04_clean_param_df.R"))
  
  # ** 2.1 Satellite Data ------------------------------------------------------
  # Extracts satellite data from Google Earth Engine. For example, extracts
  # nighttime lights, landsat data, etc.
  
  # RUN THIS USING PYTHON!
  #source(file.path(anc_fb_marketing_dir, "01_extract_values.ipynb"))
  source(file.path(anc_fb_marketing_dir, "02_append_data.R"))
  
  # ** 2.1 Sentinel 5P Pollution -----------------------------------------------
  # Extracts Sentinel 5P Data
  
  source(file.path(anc_s5p_dir, "extract_s5p.R"))
  
  # ** 2.1 DMSP -----------------------------------------------
  # Extracts Sentinel 5P Data
  
  source(file.path(anc_dmspharmon_dir, "01_extract_181920_average.R"))
  source(file.path(anc_dmspharmon_dir, "01_extract_ntl_harmonized.R"))
  source(file.path(anc_dmspharmon_dir, "02_append.R"))
  
  # ** 2.2 CNN Features Predict NTL --------------------------------------------
  # Extracts features from CNN model that uses daytime imagery to predict NTL
  # DEPENDS ON: 02_get_process_ancillary_data/Satellite Data/ being run first
  # -- 01_create_ntlgroup_tfrecord_name.R: Create dataset that randomly picks
  #    survey locations for CNN (creates balanced dataset across NTL values)
  #    and groups locations together for different TF records. Adds in nighttime
  #    lights value used for CNN.
  # -- 02_extract_data_gee_for_cnn.ipynb: Extracts data used for CNN; matrix
  #    of daytime imagery and corresponding NTL value. Outputs them as tfrecords.
  # TODO:
  # -- 03_[CNN FILE]: This is run on Google Colab
  # -- 04_cnn_extract_features.ipynb: Extracts features from CNN model at each
  #   survey location
  source(file.path(anc_cnn_features_dir, "01_create_ntlgroup_tfrecord_name.R"))
  #source(file.path(anc_cnn_features_dir, "02_extract_data_gee_for_cnn.ipynb"))
  #source(file.path(anc_cnn_features_dir, "04_cnn_extract_features.ipynb"))
  
  # 3. Merge Ancillary data with Survey ----------------------------------------
  # Merges all ancillary data with survey data
  # -- 01_merge_data.R: Merges data together
  # -- 02_clean_data.R: Cleans data (variable construction, etc.) This script
  #    creates the final data final that is used for poverty estimation.
  
  source(file.path(datawork_dir, "03_merge_ancillary_data_with_survey", "01_merge_data.R"))
  source(file.path(datawork_dir, "03_merge_ancillary_data_with_survey", "02_clean_data.R"))
  
  # 4. Poverty Estimation ------------------------------------------------------
  # Machine learning models for estimating poverty and creating datasets with
  # results.
  # --
  
  source(file.path(datawork_dir, "04_poverty_estimation", "01_pov_estimation_xgboost.R"))
  source(file.path(datawork_dir, "04_poverty_estimation", "02_append_results.R"))
  source(file.path(datawork_dir, "04_poverty_estimation", "03_add_prediction_to_survey_changes.R"))
  source(file.path(datawork_dir, "04_poverty_estimation", "03_add_prediction_to_survey_levels.R"))
  source(file.path(datawork_dir, "04_poverty_estimation", "03_prediction_changes_district.R"))
  
  # 5. Tables/Figures: Global analysis -----------------------------------------
  # Makes tables and figures for paper
  # --
  
  figures_tables_global_dir <- file.path(datawork_dir, "05_figures_tables_global")
  
  ## Main
  source(file.path(figures_tables_global_dir, "main", "levels_1_correlations.R"))
  source(file.path(figures_tables_global_dir, "main", "levels_2_global_scatterplot_average_map.R"))
  source(file.path(figures_tables_global_dir, "main", "levels_3_avg_performance_by_type.R"))
  source(file.path(figures_tables_global_dir, "main", "levels_4_determinants_of_model_performance.R"))
  source(file.path(figures_tables_global_dir, "main", "levels_5_feature_importance.R"))
  source(file.path(figures_tables_global_dir, "main", "levels_6_explain_error.R"))
  
  source(file.path(figures_tables_global_dir, "main", "changes_1_correlations.R"))
  source(file.path(figures_tables_global_dir, "main", "changes_2_main_results.R"))
  source(file.path(figures_tables_global_dir, "main", "changes_3_scatter_countries.R"))
  source(file.path(figures_tables_global_dir, "main", "changes_4_explain_variation.R"))
  source(file.path(figures_tables_global_dir, "main", "changes_5_feature_importance.R"))
  source(file.path(figures_tables_global_dir, "main", "changes_6_explain_error.R"))
  
  source(file.path(figures_tables_global_dir, "main", "policyexp_1_all.R"))
  source(file.path(figures_tables_global_dir, "main", "policyexp_2_nga.R"))
  
  source(file.path(figures_tables_global_dir, "main_stats.R"))
  
  # source(file.path(figures_tables_global_dir, "main", "correlations.R"))
  # source(file.path(figures_tables_global_dir, "main", "global_scatterplot_average_map.R"))
  # source(file.path(figures_tables_global_dir, "main", "avg_performance_by_type.R"))
  # source(file.path(figures_tables_global_dir, "main", "determinants_of_model_performance.R"))
  # source(file.path(figures_tables_global_dir, "main", "compare_with_rwi.R"))
  
  ## SI
  source(file.path(figures_tables_global_dir, "si", "table_dhs_summary_both_years.R"))
  source(file.path(figures_tables_global_dir, "si", "table_dhs_summary_most_recent.R"))
  source(file.path(figures_tables_global_dir, "si", "table_wealth_sd_within_across"))
  source(file.path(figures_tables_global_dir, "si", "figure_dhsindex_globalindex_levels_cor.R"))
  #source(file.path(figures_tables_global_dir, "si", "figure_dhsindex_globalindex_changes_cor.R")) # CHECK? VALID TO DO CHANGES???
  source(file.path(figures_tables_global_dir, "si", "figure_fb_features_cor_each_country.R"))
  source(file.path(figures_tables_global_dir, "si", "figure_accuracy_featuretype_targetvar.R"))
  source(file.path(figures_tables_global_dir, "si", "figure_country_featureset_r2_levels.R"))
  source(file.path(figures_tables_global_dir, "si", "figure_educ_fb_dhs.R"))
  source(file.path(figures_tables_global_dir, "si", "figure_ml_type_comparison.R"))
  
}



