# Extract Harmonized NTL

# Load data --------------------------------------------------------------------
df <- readRDS(file.path(data_dir, SURVEY_NAME, "FinalData", "Individual Datasets", 
                        "survey_socioeconomic.Rds"))

# Delete existing files --------------------------------------------------------
if(F){
  to_rm <- file.path(data_dir, SURVEY_NAME, "FinalData", "Individual Datasets", 
                     "ntl_harmonized") %>%
    list.files(full.names = T)
  
  for(to_rm_i in to_rm) file.remove(to_rm_i)
}

# Function to Extract Globcover ------------------------------------------------
#country_code_i <- "IA"
#buffer_m <- 5000

extract_ntl_harmon <- function(df_country, 
                               year_i,
                               buffer_m){
  
  ## Project, buffer, then back to WGS
  # Go back to WGS so don't have to project larger raster
  coordinates(df_country) <- ~longitude+latitude
  crs(df_country) <- CRS("+init=epsg:4326")
  
  df_country <- geo.buffer_chunks(df_country, r = buffer_m, chunk_size = 100)
  
  if(year_i <= 1992) year_i <- 1992
  if(year_i >= 2020) year_i <- 2020
  
  if(year_i <= 2013){
    ntl <- raster(file.path(ntl_harmon_dir, "RawData", paste0("Harmonized_DN_NTL_",year_i,"_calDMSP.tif")))
  } else{
    ntl <- raster(file.path(ntl_harmon_dir, "RawData", paste0("Harmonized_DN_NTL_",year_i,"_simVIIRS.tif")))
  }
  
  ## Crop globcover
  ntl_crop <- crop(ntl, bbox(df_country))
  
  df_country[[paste0("ntlharmon_avg")]] <- exact_extract(ntl_crop, df_country, "mean")
  df_country[[paste0("ntlharmon_sd")]]  <- exact_extract(ntl_crop, df_country, "stdev")
  
  df_out <- df_country@data %>%
    dplyr::select(uid, year, contains("ntlharmon_"))
  
  return(df_out)
}

# Implement Function and Export ------------------------------------------------
for(buffer_i in c(BUFFER_SATELLITE, 1120, 3360)){
  for(country_i in unique(df$country_code)){
    
    df_country <- df[df$country_code %in% country_i,]
    
    for(year_i in unique(df_country$year)){
      print(paste0(country_i, " - ", buffer_i, " - ", year_i))
      
      OUT_PATH <- file.path(data_dir, SURVEY_NAME, "FinalData", "Individual Datasets", 
                            "ntl_harmonized", 
                            paste0("ntlharmon_", country_i, "_", buffer_i, "m_",year_i,".Rds"))
      
      if(REPLACE_IF_EXTRACTED | !file.exists(OUT_PATH)){
        df_ntl_i <- extract_ntl_harmon(df_country[df_country$year %in% year_i,], 
                                       year_i,
                                       buffer_i)
        saveRDS(df_ntl_i, OUT_PATH)
      }
      
    }
  }
}






