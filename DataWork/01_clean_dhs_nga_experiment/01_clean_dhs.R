# Clean DHS Data for Nigeria 
# Use all Nigeria rounds

# Clean DHS survey data. Create Household Level dataframe with relevant
# socioeconomic variables.

# Functions to Clean Data ------------------------------------------------------
clean_hh <- function(df, 
                     hh_vars){
  
  #### Country specific fixes
  if(grepl("KH_2014|KH_2010", df$hv000[1])){
    # DH doesn't record anything for hv201 (water source); however, has water source
    # during dry and wet times. Use dry (sh102)
    df <- df %>%
      dplyr::select(-hv201) %>%
      dplyr::rename(hv201 = sh102)
  }
  
  if(grepl("KH_2005", df$hv000[1])){
    # DH doesn't record anything for hv201 (water source); however, has water source
    # during dry and wet times. Use dry (sh102)
    df <- df %>%
      dplyr::select(-hv201) %>%
      dplyr::rename(hv201 = hv201d)
  }
  
  #### Education variables
  ## Functions
  to_na_if_30above <- function(x){
    x[x >= 30] <- NA
    return(x)
  }
  
  to_na_if_4above <- function(x){
    x[x >= 4] <- NA
    return(x)
  }
  
  max_ig_na <- function(x){
    x <- max(x, na.rm = T)
    x[x %in% c(Inf,-Inf)] <- NA
    return(x)
  }
  
  count_0 <- function(x){
    sum(x == 0, na.rm = T)
  }
  
  count_1 <- function(x){
    sum(x == 1, na.rm = T)
  }
  
  count_2 <- function(x){
    sum(x == 2, na.rm = T)
  }
  
  count_3 <- function(x){
    sum(x == 3, na.rm = T)
  }
  
  count_3g <- function(x){
    sum(x > 3, na.rm = T)
  }
  
  mean_ig_na <- function(x){
    x <- mean(x, na.rm = T)
    x[x %in% c(Inf,-Inf)] <- NA
    return(x)
  }
  
  median_ig_na <- function(x){
    x <- median(x, na.rm = T)
    x[x %in% c(Inf,-Inf)] <- NA
    return(x)
  }
  
  ## Education Completed in Single Years
  educ_years <- df %>%
    dplyr::select(contains("hv108")) %>%
    mutate_all(to_numeric) %>%
    mutate_all(as.numeric) %>%
    mutate_all(to_na_if_30above) 
  
  df$educ_years_hh_max  <- apply(educ_years, 1, max_ig_na)
  df$educ_years_hh_mean <- apply(educ_years, 1, mean_ig_na)
  
  ## Highest Education Level Obtained
  # 0 = Early childhoon education program
  # 1 = Primary
  # 2 = Secondary
  # 3 = Higher
  # 8 = Don't Know
  # 00 = Less than 1 year completed
  # 98 = Don't know
  educ_levels <- df %>%
    dplyr::select(contains("hv106")) %>%
    mutate_all(to_numeric) %>%
    mutate_all(as.numeric) %>%
    mutate_all(to_na_if_4above) 
  
  df$educ_levels_hh_max <- apply(educ_levels, 1, max_ig_na)
  df$educ_levels_hh_n0  <- apply(educ_levels, 1, count_0)
  df$educ_levels_hh_n1  <- apply(educ_levels, 1, count_1)
  df$educ_levels_hh_n2  <- apply(educ_levels, 1, count_2)
  df$educ_levels_hh_n3  <- apply(educ_levels, 1, count_3)
  df$educ_levels_hh_n3g <- apply(educ_levels, 1, count_3g)
  
  #df$educ_years_hh_mean <- apply(educ_years, 1, mean_ig_na)
  
  # Make sure has all variables, which is needed for renaming
  for(var_i in hh_vars){
    if(is.null(df[[var_i]])){
      df[[var_i]] <- NA
    }
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
    dplyr::select(hhid,
                  cluster_id, 
                  educ_levels_hh_max,
                  educ_levels_hh_n0,
                  educ_levels_hh_n1,
                  educ_levels_hh_n2,
                  educ_levels_hh_n3,
                  educ_levels_hh_n3g,
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
  files_all <- files_all[!grepl("archive", files_all)]
  
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
  
  # Levels of education
  if(TRUE %in% (df_onerow_names %>% str_detect("hv106"))){
    hh106_names <- df_onerow_names %>%
      str_subset("hv106")
  } else{
    hh106_names <- NULL
  } 
  
  hh_vars <- c("hhid",
               "hv000",
               hh108_names,
               hh106_names,
               "hv216",
               "sh110j",
               "sh110k",
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
               "hv215",
               "hv221",
               "hv009",
               "hv270",
               "hv271")
  
  if(grepl("RawData/KH", dir)){
    hh_vars <- c(hh_vars, "sh102")
  }
  
  # Sometimes not all variables are in the dataset
  hh_vars_import <- hh_vars[hh_vars %in% df_onerow_names]
  
  hh_df <- read_dta(hh_path, col_select = all_of(hh_vars_import)) %>% clean_hh(hh_vars = hh_vars)
  geo_sdf <- readOGR(geo_path) %>% clean_geo()
  
  # Merge data
  survey_df <- merge_clean(hh_df, geo_sdf)
  survey_df$country_year <- dir %>% str_replace_all(".*/", "")
  
  # If wealth index in separate dataset, merge in. In earlier DHS rounds, WI
  # was in a separate dataset and not in the Household Record dataset.
  wi_path <- files_all %>% str_subset("[A-Z]{2}WI") %>% str_subset(".dta$|.DTA$")
  if(length(wi_path) > 0){
    wi_df <- read_dta(wi_path)
    
    # For Egypt in 2000, hhids didn't match; needed to remove extra while space
    if(dir %>% str_detect("EG_2000")){
      wi_df$whhid <- wi_df$whhid %>% str_squish() 
      survey_df$hhid <- survey_df$hhid %>% str_squish() 
    }
    
    survey_df <- survey_df %>%
      left_join(wi_df, c("hhid" = "whhid")) %>%
      dplyr::mutate(wi_from_diff_dataset = T)
  }
  
  return(survey_df)
}

# Process Data -----------------------------------------------------------------
## Create vector of paths to country-year folders
countries <- file.path(dhs_nga_exp_dir, "RawData") %>% list.files()
country_year_dirs <- lapply(countries, function(country_i){
  country_year_dir <- file.path(dhs_nga_exp_dir, "RawData", country_i) %>% list.files(full.names = T)
}) %>% 
  unlist()

## Remove archive folders
country_year_dirs <- country_year_dirs[!grepl("archive", country_year_dirs)]

dhs_all_df <- map_df(country_year_dirs, process_dhs)

## Fix country code
# In some cases, India uses IN country code (when IN is Indonesia, and should be IA)
dhs_all_df$country_code <- dhs_all_df$country_year %>% substring(1,2)
dhs_all_df$year <- dhs_all_df$country_year %>% substring(4,7) %>% as.numeric()

# Export HH Level Data ---------------------------------------------------------
saveRDS(dhs_all_df, file.path(dhs_nga_exp_dir, "FinalData", "Individual Datasets", 
                              "survey_socioeconomic_hhlevel.Rds"))

