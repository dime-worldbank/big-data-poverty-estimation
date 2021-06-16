# Clean DHS Data

# Clean DHS survey data. Create Household Level 
# dataframe with relevant socioeconomic variables.

# Functions to Clean Data ------------------------------------------------------
clean_hh <- function(df){
  
  df <- df %>%
    dplyr::rename(cluster_id = hv001,
                  wealth_index = hv270,
                  wealth_index_score = hv271)
  
  df_mean <- df %>%
    dplyr::select(cluster_id, wealth_index, wealth_index_score) %>%
    group_by(cluster_id) %>%
    dplyr::summarise_all(mean, na.rm=T) %>%
    ungroup()
  
  df_nHH <- df %>%
    group_by(cluster_id) %>%
    dplyr::summarise(N_households = n()) %>%
    ungroup()
  
  df_out <- df_mean %>%
    left_join(df_nHH, by = "cluster_id")
  
  return(df_out)
}

clean_geo <- function(df){
  df <- df@data %>%
    dplyr::filter(SOURCE != "MIS") %>% # Missing lat/lon
    dplyr::rename(cluster_id = DHSCLUST,
                  uid = DHSID,
                  latitude = LATNUM,
                  longitude = LONGNUM,
                  urban_rural = URBAN_RURA,
                  year = DHSYEAR,
                  country_code = DHSCC) %>%
    dplyr::select(cluster_id, uid, latitude, longitude, urban_rural, year, country_code)
  
  return(df)
}

merge_clean <- function(hh_df, geo_df){
  
  df_out <- geo_df %>%
    left_join(hh_df, by = "cluster_id") %>%
    #dplyr::mutate(uid = paste0(country_code, year, cluster_id)) %>%
    mutate_if(is.factor, as.character) %>%
    dplyr::select(uid, cluster_id, everything())
  
  return(df_out)
}

process_dhs <- function(dir){
  # DESCRIPTION: Cleans and merges dhs dataseets
  # ARGs:
  # dir: Directory that countains DHS survey modules for a specific country and year
  
  print(dir)
  
  # List of all files for that country & year
  files_all <- file.path(dir) %>% list.files(recursive=T, full.names = T)
  
  # Grab HH and geo file paths
  hh_path <- files_all %>% str_subset("[A-Z]{2}HR") %>% str_subset(".dta$|.DTA$")
  geo_path <- files_all %>% str_subset("[A-Z]{2}GE") %>% str_subset(".shp$")
  
  # Load and clean data
  hh_df <- read_dta(hh_path, col_select = c(hv001, hv270, hv271)) %>% clean_hh()
  geo_sdf <- readOGR(geo_path) %>% clean_geo()
  
  # Merge data
  survey_df <- merge_clean(hh_df, geo_sdf)
  survey_df$country_year <- dir %>% str_replace_all(".*/", "")
  
  return(survey_df)
}

# Process Data -----------------------------------------------------------------
## Create vecotr of paths to country-year folders
countries <- file.path(dhs_dir, "RawData") %>% list.files()
country_year_dirs <- lapply(countries, function(country_i){
  country_year_dir <- file.path(dhs_dir, "RawData", country_i) %>% list.files(full.names = T)
}) %>% 
  unlist()

## Process Data
dhs_all_df <- map_df(country_year_dirs, process_dhs)

## Fix country code
# In some cases, India uses IN country code (when IN is Indonesia, and should be IA)
dhs_all_df$country_code <- dhs_all_df$country_year %>% substring(1,2)

## Add variable for most  recent
dhs_all_df <- dhs_all_df %>%
  group_by(country_code) %>%
  mutate(latest_survey_country = max(year)) %>%
  mutate(most_recent_survey = latest_survey_country == year) %>%
  ungroup() %>%
  dplyr::select(-latest_survey_country)

# Export -----------------------------------------------------------------------
## All
saveRDS(dhs_all_df, file.path(dhs_dir, "FinalData", "Individual Datasets", "survey_socioeconomic.Rds"))
write.csv(dhs_all_df, file.path(dhs_dir, "FinalData", "Individual Datasets", "survey_socioeconomic.csv"), row.names = F)

saveRDS(gdrive_file_path, file.path(secure_file_path, "Data", "DHS",  "FinalData", "survey_socioeconomic_geo.Rds"))
write.csv(gdrive_file_path, file.path(secure_file_path, "Data", "DHS",  "FinalData", "survey_socioeconomic_geo.csv"), row.names = F)

saveRDS(dhs_all_df, file.path(secure_file_path, "Data", "DHS",  "FinalData - PII", "survey_socioeconomic_geo.Rds"))
write.csv(dhs_all_df, file.path(secure_file_path, "Data", "DHS",  "FinalData - PII", "survey_socioeconomic_geo.csv"), row.names = F)

## Geo Only
df_geoonly <- dhs_all_df %>%
  dplyr::select(uid, latitude, longitude, urban_rural, most_recent_survey, country_code, year)

saveRDS(df_geoonly, file.path(secure_file_path, "Data", "DHS",  "FinalData - PII", "GPS_uid_crosswalk.Rds"))
write.csv(df_geoonly, file.path(secure_file_path, "Data", "DHS",  "FinalData - PII", "GPS_uid_crosswalk.csv"), row.names = F)








