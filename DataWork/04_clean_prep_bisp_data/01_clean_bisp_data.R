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
  mutate(adult = aq02 >= 18) %>%
  dplyr::select(uid, province, psu, locality, period, treatment, panel, 
                present11, present13, present13, present16, bispln,
                days_worked_last_month, income_last_month,
                months_worked_last_year, income_last_year,
                pscores, adult) %>%
  group_by(uid, province, psu, locality, period, treatment, panel, 
           present11, present13, present16) %>%
  summarise(hh_size = n(),
            income_last_month_N_NAs = sum(is.na(income_last_month)), 
            income_last_month = sum(income_last_month, na.rm=T),
            income_last_year_N_NAs = sum(is.na(income_last_year)), 
            income_last_year = sum(income_last_year, na.rm=T),
            pscores = mean(pscores, na.rm=T),
            N_adults = sum(adult %in% 1),
            N_children = sum(adult %in% 0)) %>%
  mutate(year = period %>% as_factor %>% as.character %>% as.numeric,
         hh_size = N_adults + N_children) %>%
  as.data.frame()

# Clean and add consumption data -----------------------------------------------
consum_fm_a <- read_dta(file.path(project_file_path, "Data", "BISP", "RawData - Deidentified",
                                "female",  "ModuleGPartA.dta")) %>%
  mutate(days = 14)
consum_fm_b <- read_dta(file.path(project_file_path, "Data", "BISP", "RawData - Deidentified",
                                  "female",  "ModuleGPartB.dta")) %>%
  mutate(days = 30)

consum_ma_b <- read_dta(file.path(project_file_path, "Data", "BISP", "RawData - Deidentified",
                                  "male",  "ModuleGPartB.dta")) %>%
  mutate(days = 30)
consum_ma_c <- read_dta(file.path(project_file_path, "Data", "BISP", "RawData - Deidentified",
                                  "male",  "ModuleGPartC.dta")) %>%
  mutate(days = 365)
consum_ma_d <- read_dta(file.path(project_file_path, "Data", "BISP", "RawData - Deidentified",
                                  "male",  "ModuleGPartD.dta")) %>%
  mutate(days = 365)

consum_all <- bind_rows(consum_fm_a,
                        consum_fm_b,
                        consum_ma_b,
                        consum_ma_c,
                        consum_ma_d)

## Total Consumption
consum_sum_all <- consum_all %>%
  dplyr::select(uid, period, v11, v21, v31, v41, days) %>%
  mutate_if(is.numeric, ~replace_na(., 0)) %>%
  mutate(consumption_total = v11 + v21 + v31 + v41) %>%
  
  ## Adjust comsumption based on days reported
  mutate(consumption_total = consumption_total*(30/days)) %>%
  
  ## Aggregate
  group_by(uid, period) %>%
  summarise(consumption_total = sum(consumption_total)) 

bisp_df <- merge(bisp_df, consum_sum_all, by = c("uid", "period"),
                  all.x=T, all.y=T)

# Add Poverty Line -------------------------------------------------------------
# CPI Index
# https://fred.stlouisfed.org/series/DDOE01PKA086NWDB
# 2011-01-01	116.18900000000000
# 2013-01-01	136.91300000000000 // 3030.32 Poverty Line
# 2014-01-01	142.79800000000000
# 2016-01-01	152.79700000000000

## Poverty Line
pov_line_2013 <- 3030.32 
pov_line_2011 <- (116.189 / 136.913)*3030.32
pov_line_2014 <- (142.798 / 136.913)*3030.32
pov_line_2016 <- (152.797 / 136.913)*3030.32

bisp_df$pov_line <- NA
bisp_df$pov_line[bisp_df$year %in% 2011] <- pov_line_2011
bisp_df$pov_line[bisp_df$year %in% 2013] <- pov_line_2013
bisp_df$pov_line[bisp_df$year %in% 2014] <- pov_line_2014
bisp_df$pov_line[bisp_df$year %in% 2016] <- pov_line_2016

# Create Additional Variables --------------------------------------------------
# Number adult equivalent
bisp_df$N_adult_equiv <- bisp_df$N_adults + bisp_df$N_children * 0.8

# Consumption per adult equivalent
bisp_df$consumption_adult_equiv <- (bisp_df$consumption_total / bisp_df$N_adult_equiv)

# Export -----------------------------------------------------------------------
saveRDS(bisp_df, file.path(project_file_path, "Data", "BISP", 
                           "FinalData", "Individual Datasets", "bisp_socioeconomic.Rds"))

write.csv(bisp_df, file.path(project_file_path, "Data", "BISP", 
                             "FinalData", "Individual Datasets", "bisp_socioeconomic.csv"), 
          row.names = F)





