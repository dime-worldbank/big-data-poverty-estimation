# Extract VIIRS Data to BISP Households

# Within a buffer, caluclates average and std dev of NTL for each month. Then
# takes the average for each year. Consequently, for each year and each buffer,
# we capture the mean and standard deviation of nighttime lights.

#### PARAMETERS
buffer_sizes_viirs_km <- c(1,5)

# Prep BISP --------------------------------------------------------------------
bisp_coords <- read.csv(file.path(secure_file_path, "Data", "BISP", "FinalData - PII", "GPS_uid_crosswalk.csv"))

coordinates(bisp_coords) <- ~longitude + latitude
crs(bisp_coords) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

# Extract VIIRS ----------------------------------------------------------------
viirs_rad <- stack(file.path(project_file_path, "Data", "VIIRS", "RawData", "VIIRS Monthly", "pakistan_viirs_monthly_avg_rad.tif"))
n_layers_viirs <- nlayers(viirs_rad)

# Initialize
month <- 4
year <- 2012
viirs_all_df <- bisp_coords@data %>% dplyr::select(uid)
for(i in 1:n_lay3ers_viirs){ 
  print(i)
  viirs_rad_i <- raster(file.path(project_file_path, "Data", "VIIRS", "RawData", "VIIRS Monthly", "pakistan_viirs_monthly_avg_rad.tif"), i) %>% velox()
  
  viirs_allbuffs_df <- lapply(buffer_sizes_viirs_km, function(buff){
    bisp_coords_buff <- gBuffer(bisp_coords, width=buff/111.12, byid=T)
    
    viirs_mean <- viirs_rad_i$extract(sp=bisp_coords_buff, fun = function(x) mean(x, na.rm = TRUE)) %>% as.data.frame
    #viirs_min <- viirs_rad_i$extract(sp=bisp_coords_buff, fun = function(x) min(x, na.rm = TRUE)) %>% as.data.frame
    #viirs_max <- viirs_rad_i$extract(sp=bisp_coords_buff, fun = function(x) max(x, na.rm = TRUE)) %>% as.data.frame
    viirs_sd <- viirs_rad_i$extract(sp=bisp_coords_buff, fun = function(x) sd(x, na.rm = TRUE)) %>% as.data.frame
    
    names(viirs_mean) <- paste0("viirs_rad_buffer_",buff,"km_", year,"_m",month,"_mean")
    #names(viirs_min) <- paste0("viirs_rad_buffer_",buff,"km_", year,"_m",month,"_min")
    #names(viirs_max) <- paste0("viirs_rad_buffer_",buff,"km_", year,"_m",month,"_max")
    names(viirs_sd) <- paste0("viirs_rad_buffer_",buff,"km_", year,"_m",month,"_sd")
    
    viirs_df <- bind_cols(viirs_mean,
                          #viirs_min,
                          #viirs_max,
                          viirs_sd)
    
    return(viirs_df)
  }) %>% bind_cols
  
  viirs_all_df <- bind_cols(viirs_all_df, viirs_allbuffs_df)
  
  month <- month + 1
  if(month == 13){
    month <- 1
    year <- year + 1
  }
  
}

#### Aggregate Months to Years -------------------------------------------------
summarise_viirs <- function(viirs_all_df, buff_i, metric_i, year_i){
  
  var_names <- paste0("viirs_rad_buffer_",buff_i,"km_",year_i,"_m",1:12,"_", metric_i)
  
  # Subset names, incase latest year and not all years available
  var_names <- var_names[var_names %in% names(viirs_all_df)]
  
  # Average
  viirs_sub_df <- viirs_all_df[,var_names] %>% 
    apply(1, mean) %>%
    as.data.frame()
  
  names(viirs_sub_df) <- paste0("viirs",
                                "_buff",buff_i,"km",
                                "_year", year_i,
                                "_spatial", metric_i %>% toupper(),
                                "_monthlyMEAN")
  
  return(viirs_sub_df)
}

#buff_i <- 1
#metric_i <- "mean"
#year_i <- 2012

df_list <- list() # Initialize list

for(buff_i in buffer_sizes_viirs_km){
  for(metric_i in c("mean", "sd")){
    for(year_i in 2012:2019){
      df <- summarise_viirs(viirs_all_df,
                      buff_i = buff_i, 
                      metric_i = metric_i, 
                      year_i = year_i)
      
      df_list <- append(df_list, df)
    }
  }
}

viirs_df <- df_list %>% bind_cols()

#### Export 
saveRDS(viirs_df, file.path(project_file_path, "Data", "BISP", "FinalData", "Individual Datasets", "bisp_viirs.Rds"))

