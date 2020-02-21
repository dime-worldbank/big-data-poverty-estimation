# Create Survey Dataset with All Variables Needed
# UID, Period + Variables

# Load Data --------------------------------------------------------------------
# Survey Poverty Data
poverty_df <- read_dta(file.path(raw_data_file_path, "BISP", "BISP - Deidentified", "UID_pscores.dta"))

# Survey Full Data
full_df <- read_dta(file.path(raw_data_file_path, "BISP", "BISP - Deidentified", "bisp_combined_plist.dta"))
full_df <- full_df %>%
  dplyr::rename(days_worked_last_month = CQ09,
                last_month_income = CQ10 ,
                months_worked_last_year = CQ11,
                last_year_income = CQ12) %>%
  dplyr::select(uid, period,
                days_worked_last_month, last_month_income, months_worked_last_year, last_year_income)

survey_data <- merge(uid_pscores, full_df, by=c("uid", "period"))
   
# Export -----------------------------------------------------------------------
saveRDS(survey_data, file.path(final_data_file_path, "BISP", "Individual Datasets", "hh_survey_socieconomic_variables.Rds"))

