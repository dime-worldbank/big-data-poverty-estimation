# Extract MOSAICKS Variables

# TODO: Create buffer around points, and add points to those. So orig survey and
# points around the original survey.

# Load data --------------------------------------------------------------------
df <- readRDS(file.path(data_dir, SURVEY_NAME, "FinalData", "Individual Datasets", 
                        "survey_socioeconomic.Rds"))

df <- df %>%
  dplyr::filter(most_recent_survey %in% T) %>%
  distinct(latitude, longitude) %>%
  dplyr::mutate(latitude  = round(round(latitude+.005,2) -.005,3),
                longitude = round(round(longitude+.005,2) -.005,3)) 

for(i in 1:2){
  df <- bind_rows(
    df,
    df %>% mutate(latitude = latitude + 0.005),
    df %>% mutate(longitude = longitude + 0.005),
    df %>% mutate(latitude = latitude - 0.005),
    df %>% mutate(longitude = longitude - 0.005)
  ) %>%
    distinct(latitude, longitude)
  
  print(nrow(df))
  
}

df_clean <- df %>%
  dplyr::rename(Latitude = latitude,
                Longitude = longitude) %>%
  dplyr::select(Latitude, Longitude) %>%
  
  # Assign coordinates to nearest tile centroid
  # https://github.com/bshenouda/mosaiks-demo/blob/main/point/points.Rmd
  dplyr::mutate(Latitude  = round(round(Latitude+.005,2) -.005,3),
                Longitude = round(round(Longitude+.005,2) -.005,3)) %>%
  
  distinct(Latitude, Longitude, .keep_all = T)

df_clean$sub <- rep(1:3, each = nrow(df_clean)/3)[1:nrow(df_clean)]

write_csv(df_clean[df_clean$sub %in% 1,], 
          file.path(data_dir, "MOSAIKS", "RawData", 
                    paste0("mosaik_input_1_", SURVEY_NAME, ".csv")))

write_csv(df_clean[df_clean$sub %in% 2,], 
          file.path(data_dir, "MOSAIKS", "RawData", 
                    paste0("mosaik_input_2_", SURVEY_NAME, ".csv")))

write_csv(df_clean[df_clean$sub %in% 3,], 
          file.path(data_dir, "MOSAIKS", "RawData", 
                    paste0("mosaik_input_3_", SURVEY_NAME, ".csv")))

