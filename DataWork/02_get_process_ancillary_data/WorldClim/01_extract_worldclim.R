# Extract WorldClim Variables

# Load data --------------------------------------------------------------------
df <- readRDS(file.path(data_dir, SURVEY_NAME, "FinalData", "Individual Datasets", "survey_socioeconomic.Rds"))

# Delete existing files --------------------------------------------------------
if(F){
  to_rm <- file.path(data_dir, SURVEY_NAME, "FinalData", "Individual Datasets", 
                     "worldclim") %>%
    list.files(full.names = T)
  
  for(to_rm_i in to_rm) file.remove(to_rm_i)
}

# Function to Extract World Clim -----------------------------------------------
extract_worldclim <- function(df_country, year_i, buffer_m){
  
  ## Project, buffer, then back to WGS
  # Go back to WGS so don't have to project larger raster
  coordinates(df_country) <- ~longitude+latitude
  crs(df_country) <- CRS("+init=epsg:4326")
  
  df_country <- geo.buffer_chunks(df_country, r = buffer_m, chunk_size = 100)
  
  ## Load world clim
  for(bio_i in 1:19){
    print(paste0("Extracting bio: ", bio_i))
    
    r <- raster(file.path(worldclim_dir, "RawData", "wc2.1_2.5m_bio", paste0("wc2.1_2.5m_bio_",bio_i,".tif")))
    r_crop <- crop(r, bbox(df_country))
    r_crop_vx <- velox(r_crop)
    df_country[[paste0("worldclim_bio_", bio_i)]] <- r_crop_vx$extract(sp = df_country, 
                                                                       fun = function(x) mean(x, na.rm=T),
                                                                       small = TRUE)[,1] %>% as.numeric()
  }
  
  df_out <- df_country@data %>%
    dplyr::select(uid, year, contains("worldclim_"))
  
  return(df_out)
}

# Implement Function and Export ------------------------------------------------
for(buffer_i in BUFFER_SATELLITE){
  for(country_i in unique(df$country_code)){
    
    df_country <- df[df$country_code %in% country_i,]
    
    for(year_i in unique(df_country$year)){
      print(paste0(country_i, " - ", buffer_i, " - ", year_i))
      
      OUT_PATH <- file.path(data_dir, SURVEY_NAME, "FinalData", "Individual Datasets", 
                            "worldclim", 
                            paste0("worldclim_", country_i, "_", buffer_i, "m_",year_i,".Rds"))
      
      if(REPLACE_IF_EXTRACTED | !file.exists(OUT_PATH)){
        df_wc_i <- extract_worldclim(df_country[df_country$year %in% year_i,], 
                                     year_i,
                                     buffer_i)
        saveRDS(df_wc_i, OUT_PATH)
      }
      
    }
  }
}







