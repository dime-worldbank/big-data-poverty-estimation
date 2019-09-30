
# Load and Prep HH Data --------------------------------------------------------------------
bisp_df <- read.csv(file.path(final_data_file_path, "BISP", "bisp_satellite_data_combined_buffer_1km.Rds"))

bisp_df <- bisp_df[!is.na(bisp_df$viirs_2012),]
bisp_df <- bisp_df[bisp_df$period %in% c(2011, 2013),]

# Transform Variables ----------------------------------------------------------
bisp_data$viirs_ln <- log(bisp_data$viirs+1)

bisp_data$hh_inc[bisp_data$hh_inc > 9000000] <- NA
bisp_data$hh_inc_ln <- log(bisp_data$hh_inc + 1)

# Regression -------------------------------------------------------------------
#### 2011 Regression
lm(hh_inc_ln ~ viirs_ln + l7_1 + l7_2 + l7_3 + l7_4 + l7_5 + l7_6 + l7_7 + l7_12 + l7_13 +
     l7_14 + l7_15 + l7_16 + l7_17 + l7_23 + l7_24 + l7_25 + l7_26 + l7_27 + l7_34 + l7_35 + l7_36 +
     l7_37 + l7_45 + l7_46 + l7_47 + l7_56 + l7_57 + l7_67, data=bisp_data[(bisp_data$period %in% 2011),]) %>% summary

lm(hh_inc_ln ~ dmspols + l7_1 + l7_2 + l7_3 + l7_4 + l7_5 + l7_6 + l7_7 + l7_12 + l7_13 +
     l7_14 + l7_15 + l7_16 + l7_17 + l7_23 + l7_24 + l7_25 + l7_26 + l7_27 + l7_34 + l7_35 + l7_36 +
     l7_37 + l7_45 + l7_46 + l7_47 + l7_56 + l7_57 + l7_67, data=bisp_data[(bisp_data$period %in% 1),]) %>% summary

bisp_data$hh_inc %>% hist
bisp_data$hh_inc[bisp_data$hh_inc < 9000000]

bisp_data$hh_inc[bisp_data$hh_inc > 9000000]

lm(normalisedscore ~ dmspols_2011, data=bisp_data) %>% summary


hist(c(bisp_data$viirs_2013 - bisp_data$viirs_2012))



bisp_data$viirs_ln[bisp_data$period %in% 2013] %>% hist

