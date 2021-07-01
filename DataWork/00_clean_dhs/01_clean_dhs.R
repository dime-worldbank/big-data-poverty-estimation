# Clean DHS Data

# Clean DHS survey data. Create Household Level 
# dataframe with relevant socioeconomic variables.

# Functions to Clean Data ------------------------------------------------------
clean_hh <- function(df){
  
  if(grepl("KH", df$hv000[1])){
    # DH doesn't record anything for hv201 (water source); however, has water source
    # during dry and wet times. Use dry (sh102)
    df <- df %>%
      dplyr::select(-hv201) %>%
      dplyr::rename(hv201 = sh102)
  }
  
  df_out <- df %>%
    dplyr::rename(cluster_id = hv001,
                  water_source = hv201,
                  floor_material = hv213,
                  toilet_type = hv205,
                  has_electricity = hv206,
                  has_radio = hv207,
                  has_tv = hv208,
                  has_fridge = hv209,
                  has_motorbike = hv211,
                  has_car = hv212,
                  n_hh_members = hv009,
                  n_rooms_sleeping = hv216,
                  wealth_index = hv270,
                  wealth_index_score = hv271) %>%
    dplyr::select(cluster_id, 
                  water_source,
                  floor_material,
                  toilet_type,
                  has_electricity,
                  has_radio,
                  has_tv,
                  has_fridge,
                  has_motorbike,
                  has_car,
                  n_hh_members,
                  n_rooms_sleeping,
                  wealth_index, wealth_index_score) %>%
    # value labels sometime different. For example, in some surveys, for floor
    # material, cement is 34 and in others cement is 35.
    mutate(floor_material = floor_material %>% as_factor() %>% as.character(),
           water_source = water_source %>% as_factor() %>% as.character(),
           toilet_type = toilet_type %>% as_factor() %>% as.character())
  
  #%>%
  #group_by(cluster_id) %>%
  #dplyr::summarise_all(mean, na.rm=T) %>%
  #ungroup()
  
  #df_nHH <- df %>%
  #  group_by(cluster_id) %>%
  #  dplyr::summarise(N_households = n()) %>%
  #  ungroup()
  
  #df_out <- df_mean %>%
  #  left_join(df_nHH, by = "cluster_id")
  
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
  
  df_out <- hh_df %>%
    left_join(geo_df, by = "cluster_id") %>%
    #mutate_if(is.factor, as.character) %>%
    mutate_at(vars(urban_rural), as.character) %>%
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
  hh_vars <- c("hv000",
               "hv001",
               "hv201",
               "hv205",
               "hv213",
               "hv206",
               "hv207",
               "hv208",
               "hv209",
               "hv211",
               "hv212",
               "hv221",
               "hv009",
               "hv216",
               "hv270",
               "hv271")
  
  if(grepl("RawData/KH", dir)){
    hh_vars <- c(hh_vars, "sh102")
  }
  
  hh_df <- read_dta(hh_path, col_select = all_of(hh_vars)) %>% clean_hh()
  geo_sdf <- readOGR(geo_path) %>% clean_geo()
  
  # Merge data
  survey_df <- merge_clean(hh_df, geo_sdf)
  survey_df$country_year <- dir %>% str_replace_all(".*/", "")
  
  return(survey_df)
}

# Process Data -----------------------------------------------------------------
## Create vector of paths to country-year folders
countries <- file.path(dhs_dir, "RawData") %>% list.files()
country_year_dirs <- lapply(countries, function(country_i){
  country_year_dir <- file.path(dhs_dir, "RawData", country_i) %>% list.files(full.names = T)
}) %>% 
  unlist()

## Remove archive folders
country_year_dirs <- country_year_dirs[!grepl("archive", country_year_dirs)]

## Process Data
dhs_all_df <- map_df(country_year_dirs, process_dhs)

## Fix country code
# In some cases, India uses IN country code (when IN is Indonesia, and should be IA)
dhs_all_df$country_code <- dhs_all_df$country_year %>% substring(1,2)
dhs_all_df$year <- dhs_all_df$country_year %>% substring(4,7) %>% as.numeric()

# Export HH Level Data ---------------------------------------------------------
saveRDS(dhs_all_df, file.path(dhs_dir, "FinalData", "Individual Datasets", "survey_socioeconomic_hhlevel.Rds"))

