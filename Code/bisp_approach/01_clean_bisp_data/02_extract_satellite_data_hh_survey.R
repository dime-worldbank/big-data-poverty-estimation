# Create Survey Dataset with All Variables Needed
# UID, Period + Variables

# Prep Survey Data --------------------------------------------------------------------
coordinates <- read_dta("~/Desktop/GPS_uid_crosswalk.dta")
coordinates <- coordinates[!is.na(coordinates$GPSN),]

coordinates_extractid_crosswalk <- read.csv(file.path(raw_data_file_path, "Landsat", "bisp_households", "bisp_hh_cluster_id_crosswalk.csv"))

coords_crosswalk <- merge(coordinates, coordinates_extractid_crosswalk, by="uid")

# Coordinates to Degrees -------------------------------------------------------
get_lat_lon <- function(number){
  deg <- floor(number / 100)
  min <- floor(number - (100*deg))
  sec <- 100 * (number - (100 * deg) - min)
  degree <- deg + (min/60) + (sec/3600)
  
  return(degree)
}

coords_crosswalk$latitude <- get_lat_lon(coords_crosswalk$GPSN)


# Extract VIIRS ----------------------------------------------------------------

# Extract DMSP-OLS -------------------------------------------------------------

# Extract Landsat --------------------------------------------------------------




coordinates_extractid_crosswalk$uid

coordinates_extractid_crosswalk <- read.csv(file.path(raw_data_file_path, "Landsat", "bisp_households", "bisp_cluster_coords.csv"))


# Survey Poverty Data
uid_pscores <- read_dta(file.path(raw_data_file_path, "BISP", "BISP - Deidentified", "UID_pscores.dta"))

# Survey Full Data
uid_pscores <- read_dta(file.path(raw_data_file_path, "BISP", "BISP - Deidentified", "bisp_combined_plist.dta"))
uid_pscores <- uid_pscores %>%
  dplyr::rename(days_worked_last_month = CQ09,
                last_month_income = CQ10 ,
                months_worked_last_year = CQ11,
                last_year_income = CQ12) %>%
  dplyr::select(uid, period,
                days_worked_last_month, last_month_income, months_worked_last_year, last_year_income)

survey_data <- merge(uid_pscores, uid_pscores, by=c("uid", "period"))
   
# Export -----------------------------------------------------------------------
saveRDS(survey_data, file.path(final_data_file_path, "BISP", "Individual Datasets", "hh_survey_socieconomic_variables.Rds"))

