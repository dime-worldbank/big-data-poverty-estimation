# Extract MOSAICKS Variables

# Load data --------------------------------------------------------------------
df <- readRDS(file.path(data_dir, SURVEY_NAME, "FinalData", "Individual Datasets", 
                        "survey_socioeconomic.Rds"))

df_clean <- df %>%
  dplyr::rename(Latitude = latitude,
                Longitude = longitude) %>%
  dplyr::select(Latitude, Longitude, uid) %>%
  
  # Assign coordinates to nearest tile centroid
  # https://github.com/bshenouda/mosaiks-demo/blob/main/point/points.Rmd
  dplyr::mutate(Latitude  = round(round(Latitude+.005,2) -.005,3),
                Longitude = round(round(Longitude+.005,2) -.005,3)) %>%
  
  distinct(Latitude, Longitude, .keep_all = T)

write_csv(df_clean, file.path(data_dir, "MOSAIKS", "RawData", 
                              paste0("mosaik_input_", SURVEY_NAME, ".csv")))


