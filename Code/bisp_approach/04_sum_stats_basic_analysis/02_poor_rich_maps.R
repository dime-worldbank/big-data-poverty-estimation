# Satellite Images of Poor and Wealthy Areas

# Load and Prep HH Data --------------------------------------------------------------------
bisp_df <- read.csv(file.path(final_data_file_path, "BISP", "bisp_satellite_data_combined_buffer_1km.Rds"))
bisp_uid_cluster_crosswalk <- read.csv(file.path(raw_data_file_path, "Landsat", "bisp_households", "bisp_hh_cluster_id_crosswalk.csv"))

bisp_df <- merge(bisp_df, bisp_uid_cluster_crosswalk, by="uid")

bisp_df <- bisp_df[bisp_df$period %in% c(2011, 2013),]

# Collapse Data to Cluster Level -----------------------------------------------
# Determine poorest and richest clusters

### Sum income by household
bisp_df_hh <- summaryBy(hh_inc ~ uid + period + cluster_id, data=bisp_df, FUN=sum, na.rm=T, keep.names=T)
bisp_df_hh$hh_inc[bisp_df_hh$hh_inc == 0] <- NA

### Average income by cluster
bisp_df_cluster <- summaryBy(hh_inc ~ cluster_id + period, data=bisp_df_hh, FUN=mean, na.rm=T, keep.names=T)

bisp_df_cluster <- bisp_df_cluster[!is.na(bisp_df_cluster$hh_inc),]

# Load Satellite Images --------------------------------------------------------
bisp_df_cluster_2011 <- bisp_df_cluster[bisp_df_cluster$period %in% 2011,]
bisp_df_cluster_2013 <- bisp_df_cluster[bisp_df_cluster$period %in% 2013,]

l7_poor_2013 <- stack(file.path(raw_data_file_path, "Landsat", "bisp_households", "2013","stacked", "713.tif"))
l7_rich_2013 <- stack(file.path(raw_data_file_path, "Landsat", "bisp_households", "2013","stacked", "1942.tif"))

l7_poor_2013_ndvi <- (l7_poor_2013[[4]] - l7_poor_2013[[1]]) / (l7_poor_2013[[4]] + l7_poor_2013[[1]])
l7_rich_2013_ndvi <- (l7_rich_2013[[4]] - l7_rich_2013[[1]]) / (l7_rich_2013[[4]] + l7_rich_2013[[1]])


plotRGB(l7_poor_2013, r=3,g=2,b=1, scale=10000)
plotRGB(l7_rich_2013, r=3,g=2,b=1, scale=10000)



bisp_uid_cluster_crosswalk$uid %in% bisp_df$uid


bisp_df$uid[bisp_df$hh_inc]


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

