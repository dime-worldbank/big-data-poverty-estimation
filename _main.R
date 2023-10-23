# Global poverty estimation using private and public sector big data sources
# Main Script

#### PARAMETERS
# Whether to run code for analysis and producing tables, figures & stats
RUN_CODE <- T

# It takes >1 day to run the ML models. The code checks which models have not 
# been run, and only runs those that have not been run. Consequently, deleting
# all models means the code will start from scratch.
DELETE_ML_RESULTS <- F

# Export a text file that summarizes how long the code took to run
EXPORT_TXT_REPORT_CODE_DURATION <- F
START_TIME <- Sys.time() # To track time for running code

# Root Directories -------------------------------------------------------------
#### Root Paths
# * Dropbox [dropbox_dir]: Where data are stored
# * Github [github_dir]: Github Repo
# * Tables/Figures [overleaf_global_dir]: Path for tables and figures for paper

dropbox_dir          <- "~/Dropbox/World Bank/IEs/Big Data Poverty Estimation"
github_dir           <- "~/Documents/Github/big-data-poverty-estimation"
#overleaf_global_dir  <- "~/Dropbox/Apps/Overleaf/Poverty Estimation - Global Paper"
overleaf_global_dir  <- file.path(github_dir, "Paper Tables and Figures")

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

# Create Directory Structure for Survey Data -----------------------------------
for(survey_name_i in c("DHS", "LSMS", "DHS_nga_policy_experiment")){
  
  ### DROPBOX
  file.path(data_dir, survey_name_i) %>% dir.create()
  file.path(data_dir, survey_name_i, "FinalData") %>% dir.create()
  
  # FinalData
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
  
  file.path(data_dir, survey_name_i, "FinalData", "pov_estimation_results", "prediction") %>% dir.create()
  file.path(data_dir, survey_name_i, "FinalData", "pov_estimation_results", "model") %>% dir.create()

  file.path(data_dir, survey_name_i, "FinalData", "cnn_features", "split_into_data_subsets") %>% dir.create()
}

# Parameters -------------------------------------------------------------------
BUFFER_OSM       <- 5000
BUFFER_SATELLITE <- 2500

# Packages ---------------------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse,
               rgdal,
               viridis,
               readstata13,
               dplyr,
               data.table,
               raster,
               stargazer,
               stringdist,
               tmaptools,
               stringr,
               geosphere,
               rgeos,
               haven,
               ggmap,
               velox,
               sf,
               sp,
               glmnet,
               rgeos,
               caret,
               mltest,
               RANN,
               lubridate,
               jsonlite,
               httr,
               curl,
               ggpmisc,
               haven,
               sjmisc,
               dbscan,
               ggplot2,
               spatialEco,
               geosphere,
               radiant.data,
               readxl,
               mclust,
               missMDA,
               DescTools,
               furrr,
               countrycode,
               FactoMineR,
               progressr,
               ggmap,
               ggridges,
               ggpubr,
               xgboost,
               WDI,
               scales,
               ggExtra,
               ggrepel,
               ggcorrplot,
               rnaturalearth,
               ggthemes,
               gghalves,
               ggtext,
               ggsignif,
               LiblineaR,
               caret,
               exactextractr)

source(file.path(github_dir, "Functions", "functions.R"))

source("https://raw.githubusercontent.com/ramarty/download_blackmarble/main/R/download_blackmarble.R")
source("https://raw.githubusercontent.com/ramarty/fast-functions/master/R/functions_in_chunks.R")
source("https://raw.githubusercontent.com/ramarty/rSocialWatcher/52eede6cf561a74584503846eb78ee8bc8fa780b/R/main.R")

# Run Scripts ------------------------------------------------------------------

#### Paths for code
datawork_dir         <- file.path(github_dir, "DataWork")
anc_dir              <- file.path(datawork_dir, "02_get_process_ancillary_data")
anc_globcover_dir    <- file.path(anc_dir, "Globcover")
anc_worldclim_dir    <- file.path(anc_dir, "WorldClim")
anc_fb_marketing_dir <- file.path(anc_dir, "Facebook Marketing")
anc_fb_rwi_dir       <- file.path(anc_dir, "Facebook Relative Wealth Index")
anc_osm_dir          <- file.path(anc_dir, "OSM")
anc_wdi_dir          <- file.path(anc_dir, "WDI")
anc_satellite_dir    <- file.path(anc_dir, "Satellite Data")
anc_cnn_features_dir <- file.path(anc_dir, "CNN Features Predict NTL")
anc_s5p_dir          <- file.path(anc_dir, "Sentinel 5P Pollution")
anc_dmspharmon_dir   <- file.path(anc_dir, "DMSPOLS_VIIRS_HARMONIZED")

#### RUN CODE
if(F){
  
  # Default to DHS
  SURVEY_NAME <- "DHS"
  
  # 0. Download GADM -----------------------------------------------------------
  # We use GADM across multiple files, so we download.
  # -- 01_download_gadm.R: downloads GADM up to ADM2.
  # -- 02_clean_adm2.R: cleans the data. Some countries don't have ADM2; in these
  #    cases, to standardize the data, we assign the ADM2 name to just be the
  #    ADM1 name.
  source(file.path(datawork_dir, "00_download_gadm", "01_download_gadm.R"))
  source(file.path(datawork_dir, "00_download_gadm", "02_clean_adm2.R"))
  
  # 1. Clean DHS Data ----------------------------------------------------------
  # 1. Append and initial cleaning of DHS
  # 2. Construct variables
  # 3. Make random folds for cross validation
  # 4. Merge folds with data
  source(file.path(datawork_dir, "01_clean_dhs", "01_clean_dhs.R"))
  source(file.path(datawork_dir, "01_clean_dhs", "02_clean_dhs_varconstruction.R"))
  source(file.path(datawork_dir, "01_clean_dhs", "03_make_within_country_folds.R"))
  source(file.path(datawork_dir, "01_clean_dhs", "04_merge_data_with_folds.R"))
  
  # 1. Clean DHS Nigeria Data --------------------------------------------------
  # 1. Clean/append data
  # 2. Variable construction
  # 3. Make folds for cross validation
  source(file.path(datawork_dir, "01_clean_lsms", "01_clean_lsms.R"))
  source(file.path(datawork_dir, "01_clean_lsms", "02_make_within_country_folds.R"))
  source(file.path(datawork_dir, "01_clean_lsms", "03_merge_data_with_folds.R"))
  
  # 1. Clean LSMS Data ----------------------------------------------------------
  # 1. Clean LSMS Data
  # 2. Make random folds for cross validation
  # 3. Merge folds with data
  source(file.path(datawork_dir, "01_clean_lsms", "01_clean_lsms.R"))
  source(file.path(datawork_dir, "01_clean_lsms", "02_make_within_country_folds.R"))
  source(file.path(datawork_dir, "01_clean_lsms", "03_merge_data_with_folds.R"))
  
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
  
  # ** 2.1 WDI -----------------------------------------------------------
  # Extract country-level WDI data
  source(file.path(anc_wdi_dir, "download_wdi.R"))
  
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
  
  # ** 2.1 Satellite Data ------------------------------------------------------
  # Extracts satellite data from Google Earth Engine. For example, extracts
  # nighttime lights, landsat data, etc.
  
  # RUN THIS USING PYTHON!
  #source(file.path(anc_fb_marketing_dir, "01_extract_values.ipynb"))
  source(file.path(anc_fb_marketing_dir, "02_append_data.R"))
  
  # ** 2.1 Sentinel 5P Pollution -----------------------------------------------
  # Extracts Sentinel 5P Data
  
  # RUN THIS IN GOOGLE EARTH ENGINE CODE EDITOR
  #source(file.path(anc_s5p_dir, "01_download_s5p.js"))
  source(file.path(anc_s5p_dir, "02_extract_s5p.R"))
  
  # ** 2.1 DMSP ----------------------------------------------------------------
  # Extracts DMSP Data
  
  source(file.path(anc_dmspharmon_dir, "01_extract_181920_average.R"))
  source(file.path(anc_dmspharmon_dir, "01_extract_ntl_harmonized.R"))
  source(file.path(anc_dmspharmon_dir, "02_append.R"))
  
  # ** 2.2 CNN Features Predict NTL --------------------------------------------
  # Extracts features from CNN model that uses daytime imagery to predict NTL
  # DEPENDS ON: 02_get_process_ancillary_data/Satellite Data/ being run first
  # -- 01_create_ntlgroup_tfrecord_name_[].R: Create dataset that randomly picks
  #    survey locations for CNN (creates balanced dataset across NTL values)
  #    and groups locations together for different TF records. Adds in nighttime
  #    lights value used for CNN.
  # -- 02_extract_data_gee_for_cnn.ipynb: Extracts data used for CNN; matrix
  #    of daytime imagery and corresponding NTL value. Outputs them as tfrecords.
  # -- 03_estimate_cnn_and_extract_features.ipynb: Runs CNN and extracts features 
  # from CNN model at each survey location
  # -- 04_pca.R: Computes PCA of CNN features
  source(file.path(anc_cnn_features_dir, "01_create_ntlgroup_tfrecord_name_ntlharmon.R"))
  source(file.path(anc_cnn_features_dir, "01_create_ntlgroup_tfrecord_name_viirsbm.R"))
  source(file.path(anc_cnn_features_dir, "01_create_ntlgroup_tfrecrod_name_viirs.R"))
  source(file.path(anc_cnn_features_dir, "02_extract_data_gee_for_cnn.ipynb"))
  source(file.path(anc_cnn_features_dir, "03_estimate_cnn_and_extract_features.ipynb"))
  source(file.path(anc_cnn_features_dir, "04_pca.R"))
  
  # 3. Merge Ancillary data with Survey ----------------------------------------
  # Merges all ancillary data with survey data
  # -- 01_merge_data.R: Merges data together
  # -- 02_clean_data.R: Cleans data (variable construction, etc.) This script
  #    creates the final data final that is used for poverty estimation.
  
  source(file.path(datawork_dir, "03_merge_ancillary_data_with_survey", "01_merge_data.R"))
  source(file.path(datawork_dir, "03_merge_ancillary_data_with_survey", "02_clean_data.R"))
}

if(RUN_CODE){
  
  set.seed(42)
  
  # 4. Poverty Estimation ------------------------------------------------------
  # Machine learning models for estimating poverty and creating datasets with
  # results.
  
  source(file.path(datawork_dir, "04_poverty_estimation", "01_pov_estimation.R"))
  source(file.path(datawork_dir, "04_poverty_estimation", "02_append_results.R"))
  source(file.path(datawork_dir, "04_poverty_estimation", "03_add_prediction_to_survey_changes.R"))
  source(file.path(datawork_dir, "04_poverty_estimation", "03_add_prediction_to_survey_levels.R"))
  source(file.path(datawork_dir, "04_poverty_estimation", "03_prediction_changes_district.R"))
  
  # 5. Tables/Figures: Global analysis -----------------------------------------
  # Makes tables and figures for paper
  # --
  
  figures_tables_global_dir <- file.path(datawork_dir, "05_figures_tables_global")
  
  ## Main Analysis
  source(file.path(figures_tables_global_dir, "main", "levels_1_correlations.R"))
  source(file.path(figures_tables_global_dir, "main", "levels_2_global_scatterplot_average_map.R"))
  source(file.path(figures_tables_global_dir, "main", "levels_3_avg_performance_by_type.R"))
  source(file.path(figures_tables_global_dir, "main", "levels_4_determinants_of_model_performance.R"))
  source(file.path(figures_tables_global_dir, "main", "levels_5_explain_error.R"))
  
  source(file.path(figures_tables_global_dir, "main", "changes_1_correlations.R"))
  source(file.path(figures_tables_global_dir, "main", "changes_2_main_results.R"))
  source(file.path(figures_tables_global_dir, "main", "changes_3_scatter_countries.R"))
  source(file.path(figures_tables_global_dir, "main", "changes_4_explain_variation.R"))
  source(file.path(figures_tables_global_dir, "main", "changes_5_explain_error.R"))
  
  source(file.path(figures_tables_global_dir, "main", "policy_exp_nigeria.R"))
  
  source(file.path(figures_tables_global_dir, "main", "lsms_1_pov_measure_cor.R"))
  source(file.path(figures_tables_global_dir, "main", "lsms_2_scatter.R"))
  source(file.path(figures_tables_global_dir, "main", "lsms_3_feature_type.R"))
  
  source(file.path(figures_tables_global_dir, "stats.R"))
  
  ## SI
  source(file.path(figures_tables_global_dir, "si", "figure_country_featureset_r2_levels.R"))
  source(file.path(figures_tables_global_dir, "si", "figure_dhsindex_globalindex_levels_cor.R"))
  source(file.path(figures_tables_global_dir, "si", "figure_educ_fb_dhs.R"))
  source(file.path(figures_tables_global_dir, "si", "figure_fb_features_cor_each_country.R"))
  source(file.path(figures_tables_global_dir, "si", "figure_ml_type_comparison.R"))
  source(file.path(figures_tables_global_dir, "si", "figure_scatter_continent.R"))
  source(file.path(figures_tables_global_dir, "si", "table_dhs_summary_both_years.R"))
  source(file.path(figures_tables_global_dir, "si", "table_dhs_summary_most_recent.R"))
  source(file.path(figures_tables_global_dir, "si", "table_wealth_sd_within_across.R"))
}

# Export time code took to run -------------------------------------------------
if(EXPORT_TXT_REPORT_CODE_DURATION){
  END_TIME <- Sys.time()
  
  sink(file.path(github_dir, "last_code_run_time.txt"))
  cat("Details from latest time script was run \n")
  cat("\n")
  cat("START TIME: ", as.character(START_TIME), "\n", sep = "")
  cat("END TIME: ", as.character(END_TIME), "\n", sep = "")
  cat("DURATION: ",
      difftime(END_TIME, START_TIME, units = "mins") %>%
        as.numeric() %>%
        round(2),
      " minutes \n", sep = "")
  cat("\n")
  cat("PARAMETERS\n")
  cat("RUN_CODE: ", RUN_CODE, "\n", sep = "")
  cat("DELETE_ML_RESULTS: ", DELETE_ML_RESULTS, "\n", sep = "")

  sink()
}


