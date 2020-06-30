# Merge Data and Prepare Final Dataset
# Produce _WIDE and _LONG dataset

# DESCRIPTION
# This script merges BISP HH data with spatial datasets extract to BISP HHs, 
# including landsat and VIIRS. 

# 1. Load data and prep HH Survey Data
# 2. Convert VIIRs to Panel
# 3. Create panel for landsat
  # 3.1 Create indices from landsat
# 4. Merge datasets & export 

# Load Data --------------------------------------------------------------------
bisp_df <- readRDS(file.path(final_data_file_path, "BISP", "Individual Datasets", "bisp_socioeconomic.Rds"))

landsat_df <- readRDS(file.path(final_data_file_path, "BISP", "Individual Datasets", "bisp_landsat.Rds"))
viirs_all_df <- readRDS(file.path(final_data_file_path, "BISP", "Individual Datasets", "bisp_viirs.Rds"))
osm_df <- readRDS(file.path(final_data_file_path, "BISP", "Individual Datasets", "bisp_osm_roads.Rds"))
facebook_df <- readRDS(file.path(final_data_file_path, "BISP", "Individual Datasets", "facebook_marketing_extract_allmerged.Rds"))

bisp_df$uid <- bisp_df$uid %>% as.character() %>% as.numeric
landsat_df$uid <- landsat_df$uid %>% as.character() %>% as.numeric
viirs_all_df$uid <- viirs_all_df$uid %>% as.character() %>% as.numeric
osm_df$uid <- osm_df$uid %>% as.character() %>% as.numeric
facebook_df$uid <- facebook_df$uid

# Prep HH Data -----------------------------------------------------------------
bisp_df$survey_round <- NA
bisp_df$survey_round[bisp_df$year %in% 2011] <- 1
bisp_df$survey_round[bisp_df$year %in% 2013] <- 2
bisp_df$survey_round[bisp_df$year %in% 2014] <- 3
bisp_df$survey_round[bisp_df$year %in% 2016] <- 4

# Create Panel for VIIRS -------------------------------------------------------
# VIIRS is in a _wide format, where we want to create a panel in a _long format.
# Here, the unit is the household where a variable is, for example: 
# viirs_rad_buffer_1km_2016_m5_mean, where:
#  []km refers to the buffer size where NTL values were extracted
#  m[] refers to the month that was extracted.
# Across montns, we take both the average and standard deviation

viirs_panel <- lapply(c(2012, 2013, 2014, 2016), function(year){
  viirs_annual_all <- viirs_all_df %>%
    dplyr::select(uid)
  for(stat in c("mean", "max", "min", "sd")){
    for(buffer in c("1km", "2km", "3km", "5km", "10km")){

      vec_mean <- viirs_all_df[,grepl(year, names(viirs_all_df)) & grepl(buffer, names(viirs_all_df)) & grepl(stat, names(viirs_all_df))] %>% apply(1, mean)
      vec_sd <- viirs_all_df[,grepl(year, names(viirs_all_df)) & grepl(buffer, names(viirs_all_df)) & grepl(stat, names(viirs_all_df))] %>% apply(1, sd)
      
      df_out <- data.frame(vec_mean, vec_sd)
      names(df_out) <- paste0("viirs_","spatial", stat,"_monthly", c("mean", "sd"),"_buff_",buffer)
      viirs_annual_all <- bind_cols(viirs_annual_all, df_out)
    }
  }
  
  viirs_annual_all$year <- year
  return(viirs_annual_all)
}) %>% bind_rows
viirs_panel$survey_round <- NA
viirs_panel$survey_round[viirs_panel$year %in% 2012] <- 1
viirs_panel$survey_round[viirs_panel$year %in% 2013] <- 2
viirs_panel$survey_round[viirs_panel$year %in% 2014] <- 3
viirs_panel$survey_round[viirs_panel$year %in% 2016] <- 4

# Create Panel for Landsat -----------------------------------------------------
# Here, we convert landsat to a panel. An example variable is: 
# b2_2016_buff_2km_min

landsat_panel <- lapply(c(2011, 2013, 2014, 2016), function(year){
  landsat_df_yyyy <- landsat_df[,c("uid",
                                    names(landsat_df)[grepl(year, names(landsat_df))] )]  
  names(landsat_df_yyyy) <- names(landsat_df_yyyy) %>% str_replace_all(paste0("_",year), "")
  landsat_df_yyyy$year <- year
  return(landsat_df_yyyy)
}) %>% bind_rows
landsat_panel$survey_round <- NA
landsat_panel$survey_round[landsat_panel$year %in% 2011] <- 1
landsat_panel$survey_round[landsat_panel$year %in% 2013] <- 2
landsat_panel$survey_round[landsat_panel$year %in% 2014] <- 3
landsat_panel$survey_round[landsat_panel$year %in% 2016] <- 4

# Add Indices to Landsat -------------------------------------------------------
indice_combinations <- combn(1:7,2)
landsat_indicie_df <- matrix(ncol=0, nrow=nrow(landsat_panel)) %>% as.data.frame

for(stat in c("mean", "min", "max")){
  for(buffer in c("_0.1km", "_0.5km", "_1km", "_1.5km", "_2km")){
    landsat_panel_i <- landsat_panel[,grepl(stat, names(landsat_panel)) & grepl(buffer, names(landsat_panel))]
    
    for(c in 1:ncol(indice_combinations)){
      band_ids <- indice_combinations[,c]
      
      band_a_values <- landsat_panel_i[,grepl(paste0("b", band_ids[1],"_"), names(landsat_panel_i))]
      band_b_values <- landsat_panel_i[,grepl(paste0("b", band_ids[2],"_"), names(landsat_panel_i))]
      
      landsat_panel_i[[paste0("b",
                              band_ids[1],
                              band_ids[2],
                              "_buff",
                              buffer,
                              "_",
                              stat
      )]] <- (band_b_values - band_a_values) / (band_b_values + band_a_values)
    }
    landsat_indicie_df <- bind_cols(landsat_indicie_df, landsat_panel_i)
  }
}
landsat_indicie_df$uid <- landsat_panel$uid
landsat_indicie_df$year <- landsat_panel$year
landsat_indicie_df$survey_round <- landsat_panel$survey_round

# Merge ------------------------------------------------------------------------
landsat_indicie_df$year <- NULL
viirs_panel$year <- NULL

bisp_satdata_df <- merge(bisp_df, landsat_indicie_df, by=c("uid", "survey_round"), all.x=T,all.y=F)
bisp_satdata_df <- merge(bisp_satdata_df, viirs_panel, by=c("uid", "survey_round"), all.x=T,all.y=F)
bisp_satdata_df <- merge(bisp_satdata_df, osm_df, by=c("uid"), all.x=T,all.y=F)
bisp_satdata_df <- merge(bisp_satdata_df, facebook_df, by=c("uid"), all.x=T,all.y=F)

# Create Variables -------------------------------------------------------------
bisp_satdata_df$pscores_poor <- bisp_satdata_df$pscores <= 16.17
bisp_satdata_df$distance_road_meters <- apply(bisp_satdata_df[,grepl("dist_osm", names(bisp_satdata_df))], 1, min) %>% as.numeric()

bisp_satdata_df$distance_road_kms <- bisp_satdata_df$distance_road_meters / 1000
bisp_satdata_df$dist_osm_fclass_residential_kms <- bisp_satdata_df$dist_osm_fclass_residential_meters / 1000

# Keep Observations where have satellite data ----------------------------------
# For some households coordinate values were NA. (ie, coordinate values in
# GPS_uid_crosswalk.dta were NA)

bisp_satdata_df$viirs_NA <- is.na(apply(bisp_satdata_df[grepl("viirs_", names(bisp_satdata_df))], 1, sum))
bisp_satdata_df$landsat_NA <- is.na(apply(bisp_satdata_df[grepl("b1", names(bisp_satdata_df))], 1, sum))
bisp_satdata_df$pscores_NA <- is.na(apply(bisp_satdata_df[grepl("pscores", names(bisp_satdata_df))], 1, sum))

# Export -----------------------------------------------------------------------
saveRDS(bisp_satdata_df, file.path(final_data_file_path, "BISP", "Merged Datasets", "bisp_socioeconomic_satellite_panel_full.Rds"))
write.csv(bisp_satdata_df, file.path(final_data_file_path, "BISP", "Merged Datasets", "bisp_socioeconomic_satellite_panel_full.csv"), row.names=F)

#### Export removing NAs
bisp_satdata_df_noNA <- bisp_satdata_df %>%
  filter(!viirs_NA & !landsat_NA & !pscores_NA)

# For now, just replace NAs with default values for facebook
bisp_satdata_df_noNA$estimate_dau_all[is.na(bisp_satdata_df_noNA$estimate_dau_all)] <- 0
bisp_satdata_df_noNA$estimate_dau_male[is.na(bisp_satdata_df_noNA$estimate_dau_male)] <- 0
bisp_satdata_df_noNA$estimate_dau_female[is.na(bisp_satdata_df_noNA$estimate_dau_female)] <- 0

bisp_satdata_df_noNA$estimate_mau_all[is.na(bisp_satdata_df_noNA$estimate_mau_all)] <- 1000
bisp_satdata_df_noNA$estimate_mau_male[is.na(bisp_satdata_df_noNA$estimate_mau_male)] <- 1000
bisp_satdata_df_noNA$estimate_mau_female[is.na(bisp_satdata_df_noNA$estimate_mau_female)] <- 1000

saveRDS(bisp_satdata_df_noNA, file.path(final_data_file_path, "BISP", "Merged Datasets", "bisp_socioeconomic_satellite_panel_full_satPovNAsRemoved.Rds"))
write.csv(bisp_satdata_df_noNA, file.path(final_data_file_path, "BISP", "Merged Datasets", "bisp_socioeconomic_satellite_panel_full_satPovNAsRemoved.csv"), row.names=F)

# One HH train
bisp_satdata_df_noNA <- bisp_satdata_df_noNA %>%
  group_by(uid) %>%
  mutate(N_uid = n()) %>%
  ungroup()

bisp_satdata_df_noNA$keep <- bisp_satdata_df_noNA$year %in% 2014
bisp_satdata_df_noNA$keep[bisp_satdata_df_noNA$N_uid %in% 1] <- TRUE 
bisp_satdata_df_noNA <- bisp_satdata_df_noNA[bisp_satdata_df_noNA$keep %in% T,]

saveRDS(bisp_satdata_df_noNA, file.path(final_data_file_path, "BISP", "Merged Datasets", "bisp_socioeconomic_satellite_panel_full_satPovNAsRemoved_1hh.Rds"))
write.csv(bisp_satdata_df_noNA, file.path(final_data_file_path, "BISP", "Merged Datasets", "bisp_socioeconomic_satellite_panel_full_satPovNAsRemoved_1hh.csv"), row.names=F)



# Check for NAs
for(var in names(bisp_satdata_df_noNA)){
  if(TRUE %in% is.na(bisp_satdata_df_noNA[[var]])){
    print(var)
  }
}


