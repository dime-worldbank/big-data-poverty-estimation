
# Load and Prep HH Data --------------------------------------------------------------------
# Satellite Data
bisp_satellite_data <- readRDS(file.path(final_data_file_path, "BISP", "bisp_satellite_data.Rds"))
bisp_satellite_data <- bisp_satellite_data[!is.na(bisp_satellite_data$viirs_2012),]
bisp_satellite_data <- bisp_satellite_data %>% as.data.frame

# BISP Data
bisp_data <- read_dta(file.path(bisp_rawdata_file_path, "BISP OPM", "bisp_combined_plist.dta"))
bisp_data <- bisp_data[bisp_data$period %in% c(0,1),]
bisp_data <- bisp_data %>%
  dplyr::select(uid, period, normalisedscore, treatment, panel, CQ10, CQ11, CQ12, LOCALITY) %>%
  dplyr::rename(hh_inc_lastmonth = CQ10,
                N_months_worked = CQ11,
                hh_inc = CQ12) %>%
  unique %>%
  as.data.frame

# Merge
bisp_data <- base::merge(bisp_data, bisp_satellite_data, by="uid", all=F)

# Prep Satellite Variables -----------------------------------------------------
bisp_data$viirs <- bisp_data$viirs_2012
bisp_data$viirs[bisp_data$period %in% 1] <- bisp_data$viirs_2013[bisp_data$period %in% 1]

bisp_data$dmspols <- bisp_data$dmspols_2011
bisp_data$dmspols[bisp_data$period %in% 1] <- bisp_data$dmspols_2013[bisp_data$period %in% 1]

band <- 1
for(band in 1:7){
  bisp_data[[paste0("l7_", band)]] <- bisp_data[[paste0("l7_2011_", band)]]
  bisp_data[[paste0("l7_", band)]][bisp_data$period %in% 1] <- bisp_data[[paste0("l7_2013_", band)]][bisp_data$period %in% 1]
}

# Transform Variables ----------------------------------------------------------
bisp_data$viirs_ln <- log(bisp_data$viirs+1)

bisp_data$hh_inc[bisp_data$hh_inc > 9000000] <- NA
bisp_data$hh_inc_ln <- log(bisp_data$hh_inc + 1)

band_combns <- combn(1:7,2)
for(i in 1:ncol(band_combns)){
  bisp_data[[paste0("l7_", band_combns[1,i], band_combns[2,i])]] <- (bisp_data[[paste0("l7_",band_combns[1,i])]] - bisp_data[[paste0("l7_",band_combns[2,i])]]) / (bisp_data[[paste0("l7_",band_combns[1,i])]] + bisp_data[[paste0("l7_",band_combns[2,i])]])
}


# Regression -------------------------------------------------------------------
#### 2011 Regression
lm(hh_inc_ln ~ dmspols + l7_1 + l7_2 + l7_3 + l7_4 + l7_5 + l7_6 + l7_7 + l7_12 + l7_13 +
     l7_14 + l7_15 + l7_16 + l7_17 + l7_23 + l7_24 + l7_25 + l7_26 + l7_27 + l7_34 + l7_35 + l7_36 +
     l7_37 + l7_45 + l7_46 + l7_47 + l7_56 + l7_57 + l7_67, data=bisp_data[(bisp_data$period %in% 0),]) %>% summary

lm(hh_inc_ln ~ dmspols + l7_1 + l7_2 + l7_3 + l7_4 + l7_5 + l7_6 + l7_7 + l7_12 + l7_13 +
     l7_14 + l7_15 + l7_16 + l7_17 + l7_23 + l7_24 + l7_25 + l7_26 + l7_27 + l7_34 + l7_35 + l7_36 +
     l7_37 + l7_45 + l7_46 + l7_47 + l7_56 + l7_57 + l7_67, data=bisp_data[(bisp_data$period %in% 1),]) %>% summary

bisp_data$hh_inc %>% hist
bisp_data$hh_inc[bisp_data$hh_inc < 9000000]

bisp_data$hh_inc[bisp_data$hh_inc > 9000000]

lm(normalisedscore ~ dmspols_2011, data=bisp_data) %>% summary








