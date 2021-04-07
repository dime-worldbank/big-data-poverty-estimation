# Alluvial Diagram of Poverty Over Time

df_district <- read_dta("/Users/robmarty/Dropbox/World Bank/IEs/Pakistan Corridors IE/Data/PSLM/FinalData/Even Years (District) Appended/pslm_appended_with_district_data.dta")



df$asset_air_coller

head(df)

cor.test(df$income_last_month, log(df$viirs_median_mean+1))

head(df)


# Load Data --------------------------------------------------------------------
bisp_df <- read.csv(file.path(project_file_path, "Data", "BISP", "FinalData", "bisp_satellite_data_combined_buffer_1km.csv"))
pscore <- read_dta(file.path(project_file_path, "Data", "BISP", "RawData - Deidentified", "UID_pscores.dta"))

pscore$period <- pscore$period %>% to_factor() %>% as.character() %>% as.numeric()

bisp_df <- merge(bisp_df, pscore, by = c("period","uid"))

bisp_df <- bisp_df[bisp_df$period %in% 2013,]

cor.test(log(bisp_df$viirs+1), log(bisp_df$hh_inc+1))

cor.test(log(bisp_df$pscores+1), log(bisp_df$dmspols_2012+1))
cor.test(bisp_df$pscores, bisp_df$hh_inc)

plot(bisp_df$hh_inc, log(bisp_df$viirs))

bisp_df

head(bisp_df)

bisp_df$period %>% table()



bisp_df$period

bisp_df$

bisp_df$viirs_2012


head(bisp_df)

