# Extracts values of landsat to BISP coordinates

#### PARAMETERS
buffer_sizes_km <- c(0.1, 1, 2)
bands <- 1:7

# Prep BISP --------------------------------------------------------------------
bisp_coords <- read.csv(file.path(secure_file_path, "Data", "BISP", "FinalData - PII", "GPS_uid_crosswalk.csv"))

coordinates(bisp_coords) <- ~longitude + latitude
crs(bisp_coords) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

## Determine which grid tile point falls in
grid <- readRDS(file.path(project_file_path, "Data", "Country Grid", "RawData", "pak_grid_200km.Rds"))
grid@data <- grid@data %>%
  dplyr::rename(tile_id = id)

bisp_coords_OVER_grid <- over(bisp_coords, grid)
bisp_coords$tile_id <- bisp_coords_OVER_grid$tile_id

# Confirm have a landsat tile for all clusters ---------------------------------
landsat_tiles <- list.files(file.path(project_file_path, "Data", "Landsat", "RawData", 2014)) %>% 
  str_replace_all(".tif|l8_2014_", "") %>%
  str_replace_all("_b*.", "") %>%
  str_replace_all("tile", "") %>%
  as.numeric() %>%
  unique()
table(bisp_coords$tile_id %in% landsat_tiles)

# Extract Landsat --------------------------------------------------------------
extract_landsat <- function(i, bisp_coords, year, bands, buffer_sizes_km){
  # For each survey point, loops through bands and extracts landsat pixel values
  # using a vector of buffers. Computes the mean, min, max and sd of values. 
  # Consequently, for each survey point, we compute the summary stats for each
  # band and for each buffer. A row is outputted for each survey location, and
  # these are appended together.
  
  # ARGS:
  # i: ith survey location
  # bisp_coords: Spatial points dataframe of survey location. Assumes has:
  #              tile_id (which landsat raster tile to load) and uid variables
  # year: year to extract landsat
  # bands: bands to extrat from landsat
  # buffer_sizes_km: buffer sizes for extracting values
  
  # RETURNS:
  # Dataframe of landsat values, one row per survey location.
  
  
  print(paste0(i, " --- ", year))
  
  # Grab all households in cluster
  bisp_coords_i <- bisp_coords[i,]
  tile_i <- bisp_coords_i$tile_id
  uid_i <- bisp_coords_i$uid
  
  #### Loop over bands - - - - - - 
  landsat_allbuffs_allbands_df <- lapply(bands, function(band_i){
    print(paste("band", band_i))
    
    r <- raster(file.path(project_file_path, "Data", "Landsat", "RawData", year, 
                          paste0("l8_",year,"_tile",tile_i,"_b",band_i,".tif"))) 
    
    #### Loop over buffers - - - - - - 
    landsat_allbuffs_df <- lapply(buffer_sizes_km, function(buffer_i){
      
      ## Buffer
      bisp_coords_i_buff <- gBuffer(bisp_coords_i, width=buffer_i/111.12, byid=T)
      
      ## Extract raster pixel values
      landsat_values <- raster::extract(r, bisp_coords_i_buff) %>% unlist()
      
      ## Summarise raster pixel values
      landsat_mean <- landsat_values %>% mean() %>% as.data.frame()
      landsat_min <- landsat_values %>% min() %>% as.data.frame()
      landsat_max <- landsat_values %>% max() %>% as.data.frame()
      landsat_sd <- landsat_values %>% sd() %>% as.data.frame()
      
      ## Name variable 
      names(landsat_mean) <- paste0("b",band_i,"_",year,"_buff_",buffer_i,"km_mean")
      names(landsat_min) <- paste0("b",band_i,"_",year,"_buff_",buffer_i,"km_min")
      names(landsat_max) <- paste0("b",band_i,"_",year,"_buff_",buffer_i,"km_max")
      names(landsat_sd) <- paste0("b",band_i,"_",year,"_buff_",buffer_i,"km_sd")
      
      ## Bind into dataframe
      landsat_df <- bind_cols(landsat_mean,
                              landsat_min,
                              landsat_max,
                              landsat_sd)
      
      return(landsat_df)
    }) %>%
      bind_cols()
    
    return(landsat_allbuffs_df)
  }) %>%
    bind_cols()
  
  landsat_allbuffs_allbands_df$uid <- uid_i
  
  return(landsat_allbuffs_allbands_df)
}

landsat_2014_df <- lapply(1:nrow(bisp_coords), extract_landsat, bisp_coords, 2014, bands, buffer_sizes_km) %>% 
  bind_rows()

saveRDS(landsat_2014_df, 
        file.path(final_data_file_path, "BISP", "Individual Datasets", "bisp_landsat_2014.Rds"))





