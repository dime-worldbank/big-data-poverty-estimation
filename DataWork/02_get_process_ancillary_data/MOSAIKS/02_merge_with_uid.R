# Merge MOSAIKS with uid

# Load data --------------------------------------------------------------------
uid_df <- read_csv(file.path(data_dir, "MOSAIKS", "RawData", 
                             paste0("mosaik_input_", SURVEY_NAME, ".csv")))

data_df <- read_csv(file.path(data_dir, "MOSAIKS", "FinalData", 
                              paste0("Mosaiks_features_", SURVEY_NAME, ".csv")))

data_df <- data_df %>%
  dplyr::rename(Latitude = Lat,
                Longitude = Lon) %>%
  rename_at(vars(-Latitude, -Longitude), ~ paste0('mosaik_', .) %>% str_replace_all("\\.\\.\\.", ""))

# Merge ------------------------------------------------------------------------
uid_c_df <- uid_df %>%
  left_join(data_df, by = c("Latitude", "Longitude"))



uid_c_df$mosaik_3 %>% is.na %>% table()


