# Merge MOSAIKS with uid

# Load data --------------------------------------------------------------------
survey_df <- readRDS(file.path(data_dir, SURVEY_NAME, "FinalData", "Individual Datasets", 
                               "survey_socioeconomic.Rds"))

survey_df <- survey_df %>%
  dplyr::filter(most_recent_survey %in% T) %>%
  dplyr::mutate(Latitude  = round(round(latitude+.005,2) -.005,3),
                Longitude = round(round(longitude+.005,2) -.005,3)) 

uid_df <- file.path(data_dir, "MOSAIKS", "RawData") %>%
  list.files(full.names = T,
             pattern = "*.csv") %>%
  str_subset(SURVEY_NAME) %>%
  map_df(read_csv) %>%
  dplyr::select(-sub)

data_df <- file.path(data_dir, "MOSAIKS", "FinalData") %>%
  list.files(full.names = T,
             pattern = "*.csv") %>%
  str_subset(SURVEY_NAME) %>%
  map_df(read_csv)

# data_df <- fread(file.path(data_dir, "MOSAIKS", "FinalData", 
#                            paste0("Mosaiks_features_", SURVEY_NAME, "_1.csv")))

# Cleanup ----------------------------------------------------------------------
data_df <- data_df %>%
  dplyr::rename(Latitude = Lat,
                Longitude = Lon) %>%
  rename_at(vars(-Latitude, -Longitude), ~ paste0('mosaik_', .) %>% str_replace_all("\\.\\.\\.", ""))

data_df <- data_df %>%
  mutate(mosaik_id = 1:n())

data_id_df <- data_df %>%
  dplyr::select(mosaik_id, Latitude, Longitude)

survey_uid_df <- survey_df %>%
  dplyr::select(uid, latitude, longitude, Latitude, Longitude)

# Merge ------------------------------------------------------------------------
uid_df <- uid_df %>%
  left_join(data_id_df, by = c("Latitude", "Longitude"))

# Separate by NA/non-NA - add nearest neighbor ---------------------------------
## Grab MOSAIKs with data; not all locations entered into MOSAIKS have data
uid_nonna_df <- uid_df %>%
  dplyr::filter(!is.na(mosaik_id))

## Merge with survey
survey_uid_df <- survey_uid_df %>%
  left_join(uid_nonna_df, by = c("Latitude", "Longitude"))

## Grab survey with no MOSAIK data
survey_uid_na_df <- survey_uid_df %>%
  dplyr::filter(is.na(mosaik_id))

## Dataset of survey with data
survey_uid_nonna_df <- survey_uid_df %>%
  dplyr::filter(!is.na(mosaik_id))

## Add nearest neighbor value
df_closest <- map_df(1:nrow(survey_uid_na_df), function(i){
  if( (i %% 20) == 0 )   print(i)
  
  survey_uid_na_df_i <- survey_uid_na_df[i,]
  
  dist <- distGeo(p1 = survey_uid_na_df_i[,c("longitude", "latitude")],
                  p2 = uid_nonna_df[,c("Longitude", "Latitude")])
  
  df_min <- uid_nonna_df[which.min(dist),]
  survey_uid_na_df_i$mosaik_id <- df_min$mosaik_id
  survey_uid_na_df_i$dist <- min(dist)
  
  return(survey_uid_na_df_i)
})

## Append
survey_mosaik_df <- bind_rows(survey_uid_nonna_df,
                              df_closest)

survey_mosaik_df <- survey_mosaik_df %>%
  left_join(data_df, by = "mosaik_id") %>%
  dplyr::select(uid, contains("mosaik_")) %>%
  dplyr::select(-mosaik_id)

# Export -----------------------------------------------------------------------
saveRDS(survey_mosaik_df, file.path(data_dir, SURVEY_NAME, "FinalData", 
                                    "Individual Datasets", "mosaik.Rds"))




