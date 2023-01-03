# Extract Black Marble Data

# Load data --------------------------------------------------------------------
df <- readRDS(file.path(data_dir, SURVEY_NAME, "FinalData", "Individual Datasets", 
                        "survey_socioeconomic.Rds"))

# Delete existing files --------------------------------------------------------
if(F){
  to_rm <- file.path(data_dir, SURVEY_NAME, "FinalData", "Individual Datasets", 
                     "blackmarble") %>%
    list.files(full.names = T) %>%
    str_subset("bm_")
  
  for(to_rm_i in to_rm) file.remove(to_rm_i)
}

# Function to Extract Black Marble Data ----------------------------------------
#country_code_i <- "IA"
#buffer_m <- 5000
extract_bm <- function(df_country, 
                       year_i,
                       buffer_m){
  
  ## Project, buffer, then back to WGS
  # Go back to WGS so don't have to project larger raster
  coordinates(df_country) <- ~longitude+latitude
  crs(df_country) <- CRS("+init=epsg:4326")
  
  df_country <- geo.buffer_chunks(df_country, r = buffer_m, chunk_size = 100)
  
  ## Load globcover
  if(year_i < 2012) year_i <- 2012
  if(year_i > 2021) year_i <- 2021
  
  r <- raster(file.path(ntl_bm_dir, "FinalData", "annual_rasters", paste0("bm_vnp46A4_",
                                                                          year_i,
                                                                          ".tif")))
  
  ## Crop globcover
  r_crop <- crop(r, bbox(df_country))
  
  df_country$viirs_bm_mean <- exact_extract(r_crop, df_country, 'mean')
  
  df_out <- df_country@data %>%
    dplyr::select(uid, year, viirs_bm_mean)
  
  return(df_out)
}

# Implement Function and Export ------------------------------------------------
for(buffer_i in c(BUFFER_SATELLITE, 1120, 3360)){
  for(country_i in unique(df$country_code)){
    
    df_country <- df[df$country_code %in% country_i,]
    
    for(year_i in unique(df_country$year)){
      print(paste0(country_i, " - ", buffer_i, " - ", year_i))
      
      OUT_PATH <- file.path(data_dir, SURVEY_NAME, "FinalData", "Individual Datasets", 
                            "blackmarble", 
                            paste0("bm_", country_i, "_", buffer_i, "m_",year_i,".Rds"))
      
      if(REPLACE_IF_EXTRACTED | !file.exists(OUT_PATH)){
        df_bm_i <- extract_bm(df_country[df_country$year %in% year_i,], 
                              year_i,
                              buffer_i)
        saveRDS(df_bm_i, OUT_PATH)
      }
      
    }
  }
}





