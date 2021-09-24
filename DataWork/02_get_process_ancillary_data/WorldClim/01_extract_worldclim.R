# Extract WorldClim Variables

replace_if_extracted <- F

# Load data --------------------------------------------------------------------
df <- readRDS(file.path(data_dir, SURVEY_NAME, "FinalData", "Individual Datasets", "survey_socioeconomic.Rds"))

# Function to Extract Globcover ------------------------------------------------
#country_code_i <- "ZM"
#buffer_m <- 2500
extract_worldclim <- function(country_code_i, buffer_m){
  
  ## Subset to country and grab year
  df_country <- df[df$country_code %in% country_code_i,]
  year_i <- df_country$year[1]
  
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
    dplyr::select(uid, contains("worldclim_"))
  
  return(df_out)
}

# Implement Function and Export ------------------------------------------------
for(buffer_i in c(2500)){
  for(country_i in sort(unique(df$country_code))){
    print(paste0(country_i, " - ", buffer_i))
    
    OUT_PATH <- file.path(data_dir, SURVEY_NAME, "FinalData", "Individual Datasets", 
                          "worldclim", 
                          paste0("worldclim_", country_i, "_", buffer_i, "m.Rds"))
    
    if(replace_if_extracted | !file.exists(OUT_PATH)){
      df_glob_i <- extract_worldclim(country_i, buffer_i)
      saveRDS(df_glob_i, OUT_PATH)
    }
  }
}








