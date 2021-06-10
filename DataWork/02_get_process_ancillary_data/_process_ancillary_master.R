# Process Anciallary Data: Master Script

# Main Parameters 
SURVEY_NAME <- "OPM"

RUN_CODE <- F
EXTRACT_DAYTIME_SAT_VALUES <- T
EXTRACT_VIIRS_VALUES <- T
EXTRACT_FACEBOOK_RWI <- T
EXTRACT_FACEBOOK_MAU <- T

# Create Other Objects ---------------------------------------------------------
if(SURVEY_NAME %in% "OPM"){
  SURVEY_COORDS_PATH <- file.path(secure_file_path, "Data", "OPM", "FinalData - PII", "GPS_uid_crosswalk.Rds")
  YEARS <- c(2011, 2013, 2014, 2016)
}

# Run Code ---------------------------------------------------------------------
if(RUN_CODE){
  anc_code_dir <- file.path(code_file_path, "DataWork", "02_get_process_ancillary_data")
  
  if(EXTRACT_DAYTIME_SAT_VALUES %in% T) source(file.path(anc_code_dir, "Landsat_Sentinel", "extract_values.R"))
  if(EXTRACT_VIIRS_VALUES %in% T)       source(file.path(anc_code_dir, "VIIRS", "extract_viirs_to_survey", "extract_viirs_to_survey.R"))
  if(EXTRACT_FACEBOOK_RWI %in% T)       source(file.path(anc_code_dir, "Facebook Relative Wealth Index", "merge_fb_wealth_with_survey.R"))
  
  if(EXTRACT_FACEBOOK_MAU %in% T){
    source(file.path(anc_code_dir, "Facebook", "01_locations_to_scrape.R"))
    source(file.path(anc_code_dir, "Facebook", "02_scrape_facebook_data.R"))
    source(file.path(anc_code_dir, "Facebook", "03_merge_data.R"))
  }

}

