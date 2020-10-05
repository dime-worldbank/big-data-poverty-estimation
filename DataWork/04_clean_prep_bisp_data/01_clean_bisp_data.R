# Clean BISP Deidentified Data

# Create Household Level BISP dataframe with all relevant socioeconomic variables

# Load Data --------------------------------------------------------------------
bisp_plist <- read_dta(file.path(project_file_path, "Data", "BISP", "RawData - Deidentified", "bisp_combined_plist.dta"))
bisp_povscore <- read_dta(file.path(project_file_path, "Data", "BISP", "RawData - Deidentified", "UID_pscores.dta"))

bisp_df <- merge(bisp_plist, bisp_povscore, by=c("period", "uid"))

# Clean main data --------------------------------------------------------------
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

# Clean and add consumption data -----------------------------------------------
consum_fm_a <- read_dta(file.path(project_file_path, "Data", "BISP", "RawData - Deidentified",
                                "female",  "ModuleGPartA.dta"))
consum_fm_b <- read_dta(file.path(project_file_path, "Data", "BISP", "RawData - Deidentified",
                                  "female",  "ModuleGPartB.dta"))

consum_ma_b <- read_dta(file.path(project_file_path, "Data", "BISP", "RawData - Deidentified",
                                  "male",  "ModuleGPartB.dta"))
consum_ma_c <- read_dta(file.path(project_file_path, "Data", "BISP", "RawData - Deidentified",
                                  "male",  "ModuleGPartC.dta"))
consum_ma_d <- read_dta(file.path(project_file_path, "Data", "BISP", "RawData - Deidentified",
                                  "male",  "ModuleGPartD.dta"))

consum_all <- bind_rows(consum_fm_a,
                        consum_fm_b,
                        consum_ma_b,
                        consum_ma_c,
                        consum_ma_d)

## Value variables: replace
consum_sum_all <- consum_all %>%
  dplyr::select(uid, period, v11, v21, v31, v41) %>%
  mutate_if(is.numeric, ~replace_na(., 0)) %>%
  mutate(consumption_total = v11 + v21 + v31 + v41) %>%
  group_by(uid, period) %>%
  summarise(consumption_total = sum(consumption_total)) 

bisp_df <- merge(bisp_df, consum_sum_all, by = c("uid", "period"),
                  all.x=T, all.y=T)

# Export -----------------------------------------------------------------------
saveRDS(bisp_df, file.path(project_file_path, "Data", "BISP", 
                           "FinalData", "Individual Datasets", "bisp_socioeconomic.Rds"))

write.csv(bisp_df, file.path(project_file_path, "Data", "BISP", 
                             "FinalData", "Individual Datasets", "bisp_socioeconomic.csv"), 
          row.names = F)





