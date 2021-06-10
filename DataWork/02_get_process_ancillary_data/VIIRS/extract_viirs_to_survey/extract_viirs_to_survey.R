# Extract VIIRS Data to BISP Households

# Within a buffer, caluclates average and std dev of NTL for each month. Then
# takes the average for each year. Consequently, for each year and each buffer,
# we capture the mean and standard deviation of nighttime lights.

#### PARAMETERS
buffer_sizes_viirs_m <- c(1,1000,5000,10000)

# Prep BISP --------------------------------------------------------------------
bisp_coords <- readRDS(SURVEY_COORDS_PATH)

coordinates(bisp_coords) <- ~longitude + latitude
crs(bisp_coords) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

# Extract VIIRS ----------------------------------------------------------------
viirs_df <- lapply(YEARS[YEARS >= 2012], function(year){
  print(paste("year", year))
  
  viirs_rad_i <- raster(file.path(project_file_path, "Data", "VIIRS", "RawData", "VIIRS Annual", 
                                  paste0("pak_viirs_median_",year,".tif"))) %>% 
    velox()
  
  viirs_allbuffs_df <- lapply(buffer_sizes_viirs_m, function(buff){
    print(buff)
    
    bisp_coords <- bisp_coords %>% spTransform(CRS(PAK_UTM_PROJ))
    bisp_coords_buff <- gBuffer(bisp_coords, width=buff, byid=T)
    bisp_coords_buff <- bisp_coords_buff %>% spTransform(CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
    
    viirs_mean <- viirs_rad_i$extract(sp=bisp_coords_buff, fun = function(x) mean(x, na.rm = TRUE), small = T) %>% as.data.frame
    viirs_sd <- viirs_rad_i$extract(sp=bisp_coords_buff, fun = function(x) sd(x, na.rm = TRUE), small = T) %>% as.data.frame
    
    names(viirs_mean) <- paste0("viirs_rad_buffer_",buff,"m_mean")
    names(viirs_sd) <- paste0("viirs_rad_buffer_",buff,"m_sd")
    
    viirs_df <- bind_cols(viirs_mean,
                          viirs_sd)
    
    return(viirs_df)
  }) %>% bind_cols
  
  viirs_allbuffs_df$uid  <- bisp_coords$uid
  viirs_allbuffs_df$year <- year
  
  return(viirs_allbuffs_df)
}) %>% 
  bind_rows()

# Export -----------------------------------------------------------------------
saveRDS(viirs_df, file.path(project_file_path, "Data", SURVEY_NAME, "FinalData", "Individual Datasets", "survey_viirs.Rds"))
write.csv(viirs_df, file.path(project_file_path, "Data", SURVEY_NAME, "FinalData", "Individual Datasets", "survey_viirs.csv"), row.names=F)

