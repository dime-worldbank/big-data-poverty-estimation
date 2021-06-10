# Extracts values of landsat to BISP coordinates

#### PARAMETERS
buffer_sizes_m <- c(100, 1000)
bands <- list(1,2,3,4,5,6,7,
              c(4,3))

# Prep OPM ---------------------------------------------------------------------
bisp_coords <- readRDS(SURVEY_COORDS_PATH)

coordinates(bisp_coords) <- ~longitude + latitude
crs(bisp_coords) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

## Determine which grid tile point falls in
grid <- readRDS(file.path(project_file_path, "Data", "Country Grid", "FinalData", "pak_grid_200km.Rds"))
grid@data <- grid@data %>%
  dplyr::rename(tile_id = id)

bisp_coords_OVER_grid <- over(bisp_coords, grid)
bisp_coords$tile_id <- bisp_coords_OVER_grid$tile_id

# Confirm have a landsat tile for all clusters ---------------------------------
landsat_tiles <- list.files(file.path(project_file_path, "Data", "Landsat", "l7", 2014)) %>% 
  str_replace_all(".tif|l7_2014_", "") %>%
  str_replace_all("_b*.", "") %>%
  str_replace_all("tile", "") %>%
  as.numeric() %>%
  unique()
table(bisp_coords$tile_id %in% landsat_tiles)

# Extract Landsat --------------------------------------------------------------
extract_landsat <- function(tile_i, 
                            bisp_coords, 
                            year, 
                            bands, 
                            buffer_sizes_m,
                            imagery_path,
                            imagery_suffix){
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
  # bands: list bands to extrat from landsat. If 2 bands in a list element, computes an indice
  # buffer_sizes_m: buffer sizes for extracting values
  # imagery_path: File path where .tif files are stored
  # imagery_suffix: character string at start of tif name
  
  # RETURNS:
  # Dataframe of landsat values, one row per survey location.
  
  print(paste0(tile_i, " --- ", year))
  
  # Grab all households in cluster
  bisp_coords_i <- bisp_coords[bisp_coords$tile_id %in% tile_i,]

  #### Loop over bands - - - - - - 
  landsat_allbuffs_allbands_df <- lapply(bands, function(band_i){
    print(paste("band", band_i))
    
    if(length(band_i) %in% 1){
      r <- raster(file.path(imagery_path, 
                            paste0(imagery_suffix,year,"_tile",tile_i,"_b",band_i,".tif"))) 
      band_name_i <- band_i
    }
    
    if(length(band_i) %in% 2){
      r1 <- raster(file.path(imagery_path, 
                             paste0(imagery_suffix,year,"_tile",tile_i,"_b",band_i[1],".tif"))) 
      r2 <- raster(file.path(imagery_path, 
                             paste0(imagery_suffix,year,"_tile",tile_i,"_b",band_i[2],".tif"))) 
      band_name_i <- paste0(band_i[1],"_",band_i[2])
      
      r <- r1
      r[] <- (r1[] - r2[]) / (r1[] + r2[])
    }
    
    #### Loop over buffers - - - - - - 
    landsat_allbuffs_df <- lapply(buffer_sizes_m, function(buffer_i){
      
      ## Buffer
      bisp_coords_i <- bisp_coords_i %>% spTransform(CRS(PAK_UTM_PROJ))
      bisp_coords_i_buff <- gBuffer(bisp_coords_i, width=buffer_i, byid=T)
      bisp_coords_i_buff <- bisp_coords_i_buff %>% spTransform(CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
      
      ## Extract raster pixel values
      r_vx <- velox(r)
      
      bisp_coords_i_buff[[paste0("b",band_name_i,"_buff_",buffer_i,"m_mean")]] <- 
        r_vx$extract(sp=bisp_coords_i_buff, fun=function(x){mean(x, na.rm=T)}, small = T) %>% as.numeric
      
      bisp_coords_i_buff[[paste0("b",band_name_i,"_buff_",buffer_i,"m_sd")]] <- 
        r_vx$extract(sp=bisp_coords_i_buff, fun=function(x){sd(x, na.rm=T)}, small = T) %>% as.numeric
      
      ## Bind into dataframe
      landsat_df <- bisp_coords_i_buff@data
      landsat_df$tile_id <- NULL
      landsat_df$uid <- NULL
      
      return(landsat_df)
    }) %>%
      bind_cols()
    
    return(landsat_allbuffs_df)
  }) %>%
    bind_cols()
  
  landsat_allbuffs_allbands_df$uid <- bisp_coords_i$uid
  landsat_allbuffs_allbands_df$year <- year
  
  return(landsat_allbuffs_allbands_df)
}

# Implement Function -----------------------------------------------------------
l7_2014_df <- lapply(sort(unique(bisp_coords$tile_id)), 
                          extract_landsat, bisp_coords, 2014, bands, buffer_sizes_m,
                          file.path(project_file_path, "Data", "Landsat", "l7", 2014),
                          "l7_") %>% 
  bind_rows()

saveRDS(l7_2014_df, 
        file.path(data_dir, SURVEY_NAME, "FinalData", "Individual Datasets", "survey_l7_2014.Rds"))
write.csv(l7_2014_df, 
        file.path(data_dir, SURVEY_NAME, "FinalData", "Individual Datasets", "survey_l7_2014.csv"), row.names = F)




