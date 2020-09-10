# Extract Satellite Imagery to BISP Households

# Extract Landsat and VIIRS data to BISP Households. Extract to different buffer
# sizes.

#### PARAMETERS
buffer_sizes_landsat_km <- c(0.1, 0.5, 1, 1.5, 2)
buffer_sizes_viirs_km <- c(1,2,3,5,10)

# Prep BISP --------------------------------------------------------------------
# Prep Coordinates Data
bisp_coords <- read_dta(file.path(bisp_geocodes_file_path, "GPS_uid_crosswalk.dta"))
bisp_coords <- bisp_coords[!is.na(bisp_coords$GPSN),]
bisp_coords$latitude <- get_lat_lon(bisp_coords$GPSN)
bisp_coords$longitude <- get_lat_lon(bisp_coords$GPSE)
bisp_coords$uid <- bisp_coords$uid %>% as.numeric()

# Merge with cluster IDs
bisp_cluster_crosswalk <- read.csv(file.path(bisp_geocodes_file_path, "bisp_uid_cluster_crosswalk.csv"))
bisp_coords <- merge(bisp_coords, bisp_cluster_crosswalk, by="uid")

# Spatially define
coordinates(bisp_coords) <- ~longitude+latitude
crs(bisp_coords) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

# Restrict to observations with data
bisp_coords <- bisp_coords[!(bisp_coords$cluster_id %in% c(5240)),]

# Confirm have a landsat tile for all clusters
landsat_tiles <- list.files(file.path(raw_data_file_path, "Landsat", "bisp_households", 2011, "stacked")) %>% str_replace_all(".tif", "") %>% as.numeric
table(bisp_coords$cluster_id %in% landsat_tiles)

# Extract VIIRS ----------------------------------------------------------------
viirs_rad <- stack(file.path(raw_data_file_path, "VIIRS", "VIIRS Monthly", "pakistan_viirs_monthly_avg_rad.tif"))
n_layers_viirs <- nlayers(viirs_rad)

# Initialize
month <- 4
year <- 2012
viirs_all_df <- bisp_coords@data %>% dplyr::select(uid)
for(i in 1:n_layers_viirs){ 
  print(i)
  viirs_rad_i <- raster(file.path(raw_data_file_path, "VIIRS", "VIIRS Monthly", "pakistan_viirs_monthly_avg_rad.tif"), i) %>% velox()

  viirs_allbuffs_df <- lapply(buffer_sizes_viirs_km, function(buff){
    bisp_coords_buff <- gBuffer(bisp_coords, width=buff/111.12, byid=T)
    
    viirs_mean <- viirs_rad_i$extract(sp=bisp_coords_buff, fun = function(x) mean(x, na.rm = TRUE)) %>% as.data.frame
    viirs_min <- viirs_rad_i$extract(sp=bisp_coords_buff, fun = function(x) min(x, na.rm = TRUE)) %>% as.data.frame
    viirs_max <- viirs_rad_i$extract(sp=bisp_coords_buff, fun = function(x) max(x, na.rm = TRUE)) %>% as.data.frame
    viirs_sd <- viirs_rad_i$extract(sp=bisp_coords_buff, fun = function(x) sd(x, na.rm = TRUE)) %>% as.data.frame
    
    names(viirs_mean) <- paste0("viirs_rad_buffer_",buff,"km_", year,"_m",month,"_mean")
    names(viirs_min) <- paste0("viirs_rad_buffer_",buff,"km_", year,"_m",month,"_min")
    names(viirs_max) <- paste0("viirs_rad_buffer_",buff,"km_", year,"_m",month,"_max")
    names(viirs_sd) <- paste0("viirs_rad_buffer_",buff,"km_", year,"_m",month,"_sd")
    
    viirs_df <- bind_cols(viirs_mean,
                            viirs_min,
                            viirs_max,
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

#### Export 
saveRDS(viirs_all_df, file.path(final_data_file_path, "BISP", "Individual Datasets", "bisp_viirs.Rds"))

# Extract Landsat --------------------------------------------------------------
extract_landsat <- function(cluster_id_i, bisp_coords, year, buffer_sizes_km){
  print(paste0(cluster_id_i, " --- ", year))
  
  # Grab all households in cluster
  bisp_coords_i <- bisp_coords[bisp_coords$cluster_id %in% cluster_id_i,]
  
  # Load raster stack for that cluster; extract summary stats
  r <- stack(file.path(raw_data_file_path, "Landsat", "bisp_households", year, "stacked", paste0(cluster_id_i, ".tif")))
  r_velox <- velox(r)
  
  landsat_allbuffs_df <- lapply(buffer_sizes_km, function(buff_i){
    bisp_coords_i_buff <- gBuffer(bisp_coords_i, width=buff_i/111.12, byid=T)
    
    landsat_mean <- r_velox$extract(sp=bisp_coords_i_buff, fun = function(x) mean(x, na.rm = TRUE)) %>% as.data.frame
    landsat_min <- r_velox$extract(sp=bisp_coords_i_buff, fun = function(x) min(x, na.rm = TRUE)) %>% as.data.frame
    landsat_max <- r_velox$extract(sp=bisp_coords_i_buff, fun = function(x) max(x, na.rm = TRUE)) %>% as.data.frame
    landsat_sd <- r_velox$extract(sp=bisp_coords_i_buff, fun = function(x) sd(x, na.rm = TRUE)) %>% as.data.frame
    
    names(landsat_mean) <- paste0("b",1:7,"_",year,"_buff_",buff_i,"km_mean")
    names(landsat_min) <- paste0("b",1:7,"_",year,"_buff_",buff_i,"km_min")
    names(landsat_max) <- paste0("b",1:7,"_",year,"_buff_",buff_i,"km_max")
    names(landsat_sd) <- paste0("b",1:7,"_",year,"_buff_",buff_i,"km_sd")
    
    landsat_df <- bind_cols(landsat_mean,
                            landsat_min,
                            landsat_max,
                            landsat_sd)
  }) %>% bind_cols
  
  landsat_allbuffs_df$uid <- bisp_coords_i$uid
  
  return(landsat_allbuffs_df)
}

landsat_2011_df <- lapply(unique(bisp_coords$cluster_id), extract_landsat, bisp_coords, 2011, buffer_sizes_km=buffer_sizes_landsat_km) %>% bind_rows
landsat_2013_df <- lapply(unique(bisp_coords$cluster_id), extract_landsat, bisp_coords, 2013, buffer_sizes_km=buffer_sizes_landsat_km) %>% bind_rows
landsat_2014_df <- lapply(unique(bisp_coords$cluster_id), extract_landsat, bisp_coords, 2014, buffer_sizes_km=buffer_sizes_landsat_km) %>% bind_rows
landsat_2016_df <- lapply(unique(bisp_coords$cluster_id), extract_landsat, bisp_coords, 2016, buffer_sizes_km=buffer_sizes_landsat_km) %>% bind_rows

landsat_df <- merge(landsat_2011_df, landsat_2013_df, by="uid")
landsat_df <- merge(landsat_df, landsat_2014_df, by="uid")
landsat_df <- merge(landsat_df, landsat_2016_df, by="uid")

#### Export 
saveRDS(landsat_df, file.path(final_data_file_path, "BISP", "Individual Datasets", "bisp_landsat.Rds"))





