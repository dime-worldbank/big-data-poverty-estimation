# Clean BISP Deidentified Data

# Create Household Level BISP dataframe with all relevant socioeconomic variables

# Load Data --------------------------------------------------------------------
bisp_plist <- read_dta(file.path(raw_data_file_path, "BISP", "BISP - Deidentified", "bisp_combined_plist.dta"))
bisp_povscore <- read_dta(file.path(raw_data_file_path, "BISP", "BISP - Deidentified", "UID_pscores.dta"))

bisp_df <- merge(bisp_plist, bisp_povscore, by=c("period", "uid"))

# Select variables and rename --------------------------------------------------
names(bisp_df) <- names(bisp_df) %>% tolower
bisp_df <- bisp_df %>%
  dplyr::rename(days_worked_last_month = cq09,
                income_last_month = cq10,
                months_worked_last_year = cq11,
                income_last_year = cq12) %>%
  dplyr::select(uid, province, psu, locality, period, treatment, panel, 
                present11, present13, present13, present16, bispln,
                days_worked_last_month, income_last_month,
                months_worked_last_year, income_last_year,
                pscores) %>%
  group_by(uid, province, psu, locality, period, treatment, panel, 
           present11, present13, present16) %>%
  summarise(hh_size = n(),
            income_last_month_N_NAs = sum(is.na(income_last_month)), 
            income_last_month = sum(income_last_month, na.rm=T),
            income_last_year_N_NAs = sum(is.na(income_last_year)), 
            income_last_year = sum(income_last_year, na.rm=T),
            pscores = mean(pscores, na.rm=T)) %>%
  mutate(year = period %>% as_factor %>% as.character %>% as.numeric) %>%
  as.data.frame()

# Export -----------------------------------------------------------------------
saveRDS(bisp_df, file.path(final_data_file_path, "BISP", "Individual Datasets", "bisp_socioeconomic.Rds"))





