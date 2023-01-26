# Extract Black Marble Data

# Delete existing files --------------------------------------------------------
if(F){
  to_rm <- file.path(data_dir, SURVEY_NAME, "FinalData", "Individual Datasets") %>%
    list.files(full.names = T) %>%
    str_subset("bm181920_")
  
  for(to_rm_i in to_rm) file.remove(to_rm_i)
}

# Prep NTL Data ----------------------------------------------------------------
r18 <- raster(file.path(ntl_bm_dir, "FinalData", "annual_rasters", paste0("bm_vnp46A4_",
                                                                          2018,
                                                                          ".tif")))

r19 <- raster(file.path(ntl_bm_dir, "FinalData", "annual_rasters", paste0("bm_vnp46A4_",
                                                                          2019,
                                                                          ".tif")))

r20 <- raster(file.path(ntl_bm_dir, "FinalData", "annual_rasters", paste0("bm_vnp46A4_",
                                                                          2020,
                                                                          ".tif")))

r_stack <- stack(r18, r19, r20)

r <- calc(r_stack, fun = mean, na.rm = T)

# Extract Data -----------------------------------------------------------------
buffer_i <- 1120

for(buffer_i in c(1120, 3360)){
  print(buffer_i)
  
  OUT_PATH <- file.path(data_dir, SURVEY_NAME, "FinalData", "Individual Datasets",
                        paste0("bm181920_",buffer_i,"m",".Rds"))
  
  if(!file.exists(OUT_PATH)){
    
    #### Prep Survey Data
    df <- readRDS(file.path(data_dir, SURVEY_NAME, "FinalData", "Individual Datasets",
                            "survey_socioeconomic.Rds"))
    
    coordinates(df) <- ~longitude+latitude
    crs(df) <- CRS("+init=epsg:4326")
    
    df <- geo.buffer_chunks(df, r = buffer_i, chunk_size = 100)
    
    #### Extract values
    df$viirs_bm <- exact_extract(r, df, 'mean')
    
    df_out <- df@data %>%
      dplyr::select(uid, year, viirs_bm)
    
    saveRDS(df_out, OUT_PATH)
    
  }
}
