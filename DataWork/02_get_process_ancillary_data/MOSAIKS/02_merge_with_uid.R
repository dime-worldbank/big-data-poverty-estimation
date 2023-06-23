# Merge MOSAIKS with uid

# Load / Prep Survey Data ------------------------------------------------------
## Load
survey_df <- readRDS(file.path(data_dir, SURVEY_NAME, "FinalData", "Individual Datasets", 
                               "survey_socioeconomic.Rds"))

## Prep
# - Only one time period, so only use most recent survey
# - Round coordinations to mosaik grid.
survey_df <- survey_df %>%
  dplyr::filter(most_recent_survey %in% T) %>%
  dplyr::mutate(Latitude  = round(round(latitude+.005,2) -.005,3),
                Longitude = round(round(longitude+.005,2) -.005,3)) 

## Select relevant variables
survey_uid_df <- survey_df %>%
  dplyr::select(uid, latitude, longitude, Latitude, Longitude)

# Load / Prep MOSAIK Data ------------------------------------------------------
## Load data
data_df <- file.path(data_dir, "MOSAIKS", "FinalData") %>%
  list.files(full.names = T,
             pattern = "*.csv") %>%
  str_subset(SURVEY_NAME) %>%
  map_df(read_csv)

## Cleanup
data_df <- data_df %>%
  dplyr::rename(Latitude = Lat,
                Longitude = Lon) %>%
  rename_at(vars(-Latitude, -Longitude), ~ paste0('mosaik_', .) %>% str_replace_all("\\.\\.\\.", "")) %>%
  mutate(mosaik_id = 1:n())

## Data of just IDs
data_id_df <- data_df %>%
  dplyr::select(mosaik_id, Latitude, Longitude)

# Separate by NA/non-NA - add nearest neighbor ---------------------------------

## Merge with survey
survey_uid_df <- survey_uid_df %>%
  left_join(data_id_df, by = c("Latitude", "Longitude"))

## Grab survey with no MOSAIK data
survey_uid_na_df <- survey_uid_df %>%
  dplyr::filter(is.na(mosaik_id))

## Dataset of survey with data
survey_uid_nonna_df <- survey_uid_df %>%
  dplyr::filter(!is.na(mosaik_id))

## Add nearest neighbor value
df_closest <- map_df(1:nrow(survey_uid_na_df), function(i){
  if( (i %% 20) == 0 ) print(i)
  
  survey_uid_na_df_i <- survey_uid_na_df[i,]
  
  dist <- distGeo(p1 = survey_uid_na_df_i[,c("longitude", "latitude")],
                  p2 = data_id_df[,c("Longitude", "Latitude")])
  
  df_min <- data_id_df[which.min(dist),]
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




