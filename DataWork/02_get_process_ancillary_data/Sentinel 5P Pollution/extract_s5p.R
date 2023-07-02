# Extract Data to Sentinel 5P

# Load data --------------------------------------------------------------------
dhs_sp <- readRDS(file.path(data_dir, SURVEY_NAME, "FinalData", "Individual Datasets", 
                            "survey_socioeconomic.Rds"))

# Prep DHS data ----------------------------------------------------------------
dhs_sp <- dhs_sp %>%
  dplyr::mutate(most_recent_survey %in% T) %>%
  dplyr::select(uid, latitude, longitude)

coordinates(dhs_sp) <- ~longitude+latitude
crs(dhs_sp) <- CRS("+init=epsg:4326")

dhs_buff_sp <- geo.buffer_chunks(dhs_sp, r = 2500, chunk_size = 500)

# Extract data -----------------------------------------------------------------
sat_vars <- file.path(sentinel5p_dir, "RawData") %>%
  list.files(pattern = "_3_5km.tif") %>%
  str_replace_all("_3_5km.tif", "")

dhs_sat_df <- lapply(sat_vars, function(var_i){
  
  r <- raster(file.path(sentinel5p_dir, "RawData", paste0(var_i, "_3_5km.tif"))) 
  dhs_buff_sp[[var_i]] <- exact_extract(r, dhs_buff_sp, "mean")
  
  return(dhs_buff_sp@data)
}) %>% 
  reduce(left_join, by = "uid")

# Export -----------------------------------------------------------------------
saveRDS(dhs_sat_df, file.path(data_dir, SURVEY_NAME, "FinalData", "Individual Datasets", 
                              "sentinel5p.Rds"))

