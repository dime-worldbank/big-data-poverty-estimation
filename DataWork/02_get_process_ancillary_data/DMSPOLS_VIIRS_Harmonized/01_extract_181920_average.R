# Extract Black Marble Data

# Delete existing files --------------------------------------------------------
if(F){
  to_rm <- file.path(data_dir, SURVEY_NAME, "FinalData", "Individual Datasets") %>%
    list.files(full.names = T) %>%
    str_subset("ntl_harmonized181920_")
  
  for(to_rm_i in to_rm) file.remove(to_rm_i)
}

# Prep NTL Data ----------------------------------------------------------------
r18 <- raster(file.path(ntl_harmon_dir, "RawData", paste0("Harmonized_DN_NTL_",2018,"_simVIIRS.tif")))
r19 <- raster(file.path(ntl_harmon_dir, "RawData", paste0("Harmonized_DN_NTL_",2019,"_simVIIRS.tif")))
r20 <- raster(file.path(ntl_harmon_dir, "RawData", paste0("Harmonized_DN_NTL_",2020,"_simVIIRS.tif")))

r_stack <- stack(r18, r19, r20)

r <- calc(r_stack, fun = mean, na.rm = T)

# Extract Data -----------------------------------------------------------------
buffer_i <- 1120

for(buffer_i in c(1120, 3360)){
  print(buffer_i)
  
  OUT_PATH <- file.path(data_dir, SURVEY_NAME, "FinalData", "Individual Datasets",
                        paste0("ntl_harmonized181920_",buffer_i,".Rds"))
  
  if(!file.exists(OUT_PATH)){
    
    #### Prep Survey Data
    df <- readRDS(file.path(data_dir, SURVEY_NAME, "FinalData", "Individual Datasets",
                            "survey_socioeconomic.Rds"))
    
    coordinates(df) <- ~longitude+latitude
    crs(df) <- CRS("+init=epsg:4326")
    
    df <- geo.buffer_chunks(df, r = buffer_i, chunk_size = 100)
    
    #### Extract values
    df$ntlharmon_avg <- exact_extract(r, df, 'mean')
    
    df_out <- df@data %>%
      dplyr::select(uid, year, ntlharmon_avg)
    
    saveRDS(df_out, OUT_PATH)
    
  }
}
