# Clean DHS Data

# Clean DHS survey data. Create Household Level 
# dataframe with relevant socioeconomic variables.

# Don't contain hv216 (n rooms for sleeping). These are earlier DHS rounds,
# and likely just too early to be included in this paper.
countries_to_remove <- c("MA_2003-04_DHS_09092021_1726_82518",
                         "MB_2005_DHS_09092021_1725_82518") %>%
  paste0(collapse = "|")

# Example data
#df_tmp <- read_dta("/Users/robmarty/Dropbox/World Bank/IEs/Pakistan Poverty Estimation from Satellites/Data/DHS/RawData/ZW/ZW_2015_DHS_09092021_1734_82518/ZWHR72DT/ZWHR72FL.DTA")   
#df_tmp$hv247 %>% head()

# hv204
# 996 --> 0
# 998 --> NA

# Functions to Clean Data ------------------------------------------------------
clean_hh <- function(df){
  
  #### Country specific fixes
  if(grepl("KH", df$hv000[1])){
    # DH doesn't record anything for hv201 (water source); however, has water source
    # during dry and wet times. Use dry (sh102)
    df <- df %>%
      dplyr::select(-hv201) %>%
      dplyr::rename(hv201 = sh102)
  }
  
  #### Education variables
  ## Functions
  to_na_if_large <- function(x){
    x[x >= 30] <- NA
    return(x)
  }
  
  max_ig_na <- function(x){
    x <- max(x, na.rm = T)
    x[x %in% c(Inf,-Inf)] <- NA
    return(x)
  }
  
  mean_ig_na <- function(x){
    x <- mean(x, na.rm = T)
    x[x %in% c(Inf,-Inf)] <- NA
    return(x)
  }
  
  educ_years <- df %>%
    dplyr::select(contains("hv108")) %>%
    mutate_all(to_numeric) %>%
    mutate_all(as.numeric) %>%
    mutate_all(to_na_if_large) 
  
  df$educ_years_hh_max  <- apply(educ_years, 1, max_ig_na)
  df$educ_years_hh_mean <- apply(educ_years, 1, mean_ig_na)
  
  # Earlier DHS don't have this variable
  if(is.null(df$hv216)){
    df$hv216 <- NA
  }
  
  df_out <- df %>%
    dplyr::rename(cluster_id = hv001,
                  water_source = hv201,
                  water_time_to_get = hv204,
                  floor_material = hv213,
                  toilet_type = hv205,
                  has_electricity = hv206,
                  has_radio = hv207,
                  has_tv = hv208,
                  has_fridge = hv209,
                  has_motorbike = hv211,
                  has_car = hv212,
                  n_hh_members = hv009,
                  #kitchen_is_sep_room = hv242,
                  #has_bank_account = hv247,
                  wall_material = hv214,
                  roof_material = hv215,
                  n_rooms_sleeping = hv216,
                  wealth_index = hv270,
                  wealth_index_score = hv271) %>%
    dplyr::select(cluster_id, 
                  educ_years_hh_max,
                  educ_years_hh_mean,
                  water_source,
                  water_time_to_get,
                  floor_material,
                  toilet_type,
                  has_electricity,
                  has_radio,
                  has_tv,
                  has_fridge,
                  has_motorbike,
                  has_car,
                  n_hh_members,
                  #kitchen_is_sep_room,
                  #has_bank_account,
                  wall_material,
                  roof_material,
                  n_rooms_sleeping,
                  wealth_index, wealth_index_score) %>%
    # value labels sometime different. For example, in some surveys, for floor
    # material, cement is 34 and in others cement is 35.
    mutate(floor_material = floor_material %>% as_factor() %>% as.character(),
           wall_material = wall_material %>% as_factor() %>% as.character(),
           roof_material = roof_material %>% as_factor() %>% as.character(),
           water_source = water_source %>% as_factor() %>% as.character(),
           toilet_type = toilet_type %>% as_factor() %>% as.character(),
           #has_bank_account = has_bank_account %>% as_factor() %>% as.character(),
           water_time_to_get = water_time_to_get %>% as.numeric()) %>%
    dplyr::mutate(cluster_id = cluster_id %>% as.character())
  
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
    dplyr::select(cluster_id, uid, latitude, longitude, urban_rural, year, country_code) %>%
    dplyr::mutate(cluster_id = cluster_id %>% as.character())
  
  return(df)
}

merge_clean <- function(hh_df, geo_df){
  
  df_out <- hh_df %>%
    left_join(geo_df, by = "cluster_id") %>%
    mutate_at(vars(urban_rural), as.character) %>%
    dplyr::select(uid, cluster_id, everything()) %>%
    dplyr::mutate(year = year %>% as.character())
  
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
  num_to_string <- function(x){
    if(x <= 9){
      x <- paste0("0", x)
    } else{
      x <- as.character(x)
    }
    
    return(x)
  } 
  num_to_string <- Vectorize(num_to_string)
  
  ## Load variable names; needed for hv108 (education), which includes a value
  # for each household
  df_onerow_names <- read_dta(hh_path, n_max = 1) %>% names()
  
  # Years of education
  if(TRUE %in% (df_onerow_names %>% str_detect("hv108"))){
    hh108_names <- df_onerow_names %>%
      str_subset("hv108")
  } else{
    hh108_names <- NULL
  } 
  
  if(TRUE %in% (df_onerow_names %>% str_detect("sh110j"))){
    sh110j_var <- "sh110j"
  } else{
    sh110j_var <- NULL
  } 
  
  if(TRUE %in% (df_onerow_names %>% str_detect("sh110k"))){
    sh110k_var <- "sh110k"
  } else{
    sh110k_var <- NULL
  } 
  
  # N rooms for sleeping
  if(TRUE %in% (df_onerow_names %>% str_detect("hv216"))){
    hv216_var <- "hv216"
  } else{
    hv216_var <- NULL
  } 

  hh_vars <- c("hv000",
               hh108_names,
               hv216_var,
               sh110j_var,
               sh110k_var,
               "hv001",
               "hv201",
               "hv204",
               "hv205",
               "hv213",
               "hv206",
               "hv207",
               "hv208",
               "hv209",
               "hv211",
               "hv212",
               "hv214",
               #"hv242",
               #"hv247",
               "hv215",
               "hv221",
               "hv009",
               #"hv216",
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
#country_year_dirs <- country_year_dirs[!grepl(countries_to_remove, country_year_dirs)]

dhs_all_df <- map_df(country_year_dirs, process_dhs)

## Fix country code
# In some cases, India uses IN country code (when IN is Indonesia, and should be IA)
dhs_all_df$country_code <- dhs_all_df$country_year %>% substring(1,2)
dhs_all_df$year <- dhs_all_df$country_year %>% substring(4,7) %>% as.numeric()

# Export HH Level Data ---------------------------------------------------------
saveRDS(dhs_all_df, file.path(dhs_dir, "FinalData", "Individual Datasets", "survey_socioeconomic_hhlevel.Rds"))

