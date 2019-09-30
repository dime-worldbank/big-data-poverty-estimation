# Merge Satellite Data with BISP Data
# 
# DESCRIPTION:
# Merge Satellite and BISP Data. Export as csv so can by read by python.

# Load Data --------------------------------------------------------------------
satellite_data <- readRDS(file.path(final_data_file_path, "BISP", "bisp_satellite_data_buffer_1km.Rds"))
bisp_data <- read.dta13(file.path(bisp_rawdata_file_path, "BISP OPM", "bisp_combined_plist.dta"))

# Some vars stored as dataframe
for(var in names(satellite_data)){
  satellite_data[[var]] <- satellite_data[[var]] %>% as.character()
}

# Prep and Merge ---------------------------------------------------------------

# Prep BISP Data
bisp_data <- bisp_data %>%
  dplyr::select(uid, period, LOCALITY, CQ10, CQ12) %>%
  mutate(period = case_when(period == 0 ~ 2011,
                            period == 1 ~ 2013,
                            period == 2 ~ 2014, 
                            period == 3 ~ 2016
  )) %>%   
  group_by(uid, period, LOCALITY) %>%
  summarize(hh_inc = sum(CQ12, na.rm = TRUE),
            hh_inc_lastmonth = sum(CQ10, na.rm = TRUE))

bisp_data$hh_inc[bisp_data$hh_inc %in% 0] <- NA
bisp_data$hh_inc_lastmonth[bisp_data$hh_inc_lastmonth %in% 0] <- NA

bisp_data$hh_inc[bisp_data$hh_inc >= 900000] <- NA
bisp_data$hh_inc_lastmonth[bisp_data$hh_inc_lastmonth >= 900000] <- NA

# Merge BISP with Satellite Data
bisp_data <- merge(bisp_data, satellite_data, by="uid", all.x=T, all.y=F)

# Clean Data -------------------------------------------------------------------
#### Convert from String to Numeric
for(var in names(bisp_data)[grepl("viirs|dmspols|l7_", names(bisp_data))]){
  bisp_data[[var]] <- bisp_data[[var]] %>% as.character() %>% as.numeric
}

#### Make Panel Satellite Vars
bisp_data$viirs <- bisp_data$viirs_2012
bisp_data$viirs[bisp_data$period %in% 2013] <- bisp_data$viirs_2013[bisp_data$period %in% 2013]
bisp_data$viirs[bisp_data$period %in% 2014] <- bisp_data$viirs_2013[bisp_data$period %in% 2014]
bisp_data$viirs[bisp_data$period %in% 2016] <- bisp_data$viirs_2013[bisp_data$period %in% 2016]

bisp_data$dmspols <- bisp_data$dmspols_2011
bisp_data$dmspols[bisp_data$period %in% 2013] <- bisp_data$dmspols_2013[bisp_data$period %in% 2013]
bisp_data$dmspols[bisp_data$period %in% 2014] <- NA
bisp_data$dmspols[bisp_data$period %in% 2016] <- NA

band <- 1
for(band in 1:7){
  bisp_data[[paste0("l7_", band)]] <- bisp_data[[paste0("l7_2011_", band)]]
  bisp_data[[paste0("l7_", band)]][bisp_data$period %in% 2013] <- bisp_data[[paste0("l7_2013_", band)]][bisp_data$period %in% 2013]
  bisp_data[[paste0("l7_", band)]][bisp_data$period %in% 2014] <- NA # bisp_data[[paste0("l7_2014_", band)]][bisp_data$period %in% 2014]
  bisp_data[[paste0("l7_", band)]][bisp_data$period %in% 2016] <- NA # bisp_data[[paste0("l7_2016_", band)]][bisp_data$period %in% 2016]
}

#### Combinations of Satellite Bands
band_combns <- combn(1:7,2)
for(i in 1:ncol(band_combns)){
  bisp_data[[paste0("l7_", band_combns[1,i], band_combns[2,i])]] <- (bisp_data[[paste0("l7_",band_combns[1,i])]] - bisp_data[[paste0("l7_",band_combns[2,i])]]) / (bisp_data[[paste0("l7_",band_combns[1,i])]] + bisp_data[[paste0("l7_",band_combns[2,i])]])
}

# Export -----------------------------------------------------------------------
write.csv(bisp_data, file.path(final_data_file_path, "BISP", "bisp_satellite_data_combined_buffer_1km.csv"), row.names=F)



