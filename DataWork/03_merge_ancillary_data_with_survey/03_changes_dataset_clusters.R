# Make Dataset of Changes

# Changes at DHS cluster level. Find closest cluster to create pairs; only use
# if within 10km.

# Load data --------------------------------------------------------------------
df <- readRDS(file.path(data_dir, SURVEY_NAME, "FinalData", "Merged Datasets", "survey_alldata_clean.Rds"))

# Select variables -------------------------------------------------------------
# Select relevant ID variables and variables that change over time

df <- df %>%
  dplyr::filter(year >= 2000) %>%
  dplyr::select(uid, country_code, country_name, gadm_uid, iso2, year, within_country_fold, continent_adj,
                urban_rural,
                latitude, longitude,
                pca_allvars, 
                starts_with("l7_"),
                starts_with("gc_"),
                starts_with("ntlharmon_"),
                starts_with("cnn_viirs_landsat_"),
                starts_with("weather_"),
                starts_with("pollution_aod_")) 

## Make complete dataset 
df <- df %>%
  dplyr::filter(!is.na(l7_B1),
                !is.na(pollution_aod_047))

# Choose surveys with largest year difference ----------------------------------
# 1. Only keep countries with at least two surveys
# 2. If >2 surveys, select the two that have the largest year difference [not needed now]

df <- df %>%
  dplyr::mutate(cc_year = paste(country_code, year))

country_df <- df %>%
  distinct(cc_year, country_code, year) %>%
  
  ## Must have at least two surveys
  group_by(country_code) %>%
  dplyr::mutate(n_survey = n()) %>%
  ungroup() %>%
  dplyr::filter(n_survey >= 2) %>%
  
  ## Take first and last survey for each country
  group_by(country_code) %>%
  dplyr::filter(year %in% c(min(year), max(year)))

df <- df %>%
  dplyr::filter(cc_year %in% country_df$cc_year)

# Create DHS cluster pairs -----------------------------------------------------
df_pairs <- map_df(unique(df$country_code), function(country_code_i){
  print(country_code_i)
  
  ## Grab data in one country, and separate by year
  df_c <- df[df$country_code %in% country_code_i,]
  
  df_c_yr1 <- df_c[df_c$year %in% max(df_c$year),]
  df_c_yr2 <- df_c[df_c$year %in% min(df_c$year),]
  
  ## Loop through observations in data_yr1, finding closest observation in data_yr2
  df_c_append <- map_df(1:nrow(df_c_yr1), function(i){
    
    df_c_yr1_i <- df_c_yr1[i,]
    
    dist <- sqrt((df_c_yr1_i$latitude - df_c_yr2$latitude)^2 + (df_c_yr1_i$longitude - df_c_yr2$longitude)^2)
    df_c_yr2_i <- df_c_yr2[which.min(dist),]
    
    df_append_i <- bind_rows(df_c_yr1_i, df_c_yr2_i)
    df_append_i$uid_panel <- df_c_yr1_i$uid # Unique ID for panel
    df_append_i$uid_t2    <- df_c_yr2_i$uid # ID from second time period (to check for repeats)
    
    df_append_i$urban_rural_yr1 <- df_c_yr1_i$urban_rural 
    df_append_i$urban_rural_yr2 <- df_c_yr2_i$urban_rural 
    
    ## For some variables, use data from data_yr1 for both
    for(var in c("country_code", "gadm_uid", "within_country_fold", "continent_adj", "iso2")){
      df_append_i[[var]] <- df_c_yr1_i[[var]]
    }
    
    df_append_i$dist_uids_m <- distGeo(p1 = c(df_append_i$longitude[1],
                                              df_append_i$latitude[1]),
                                       p2 = c(df_append_i$longitude[2],
                                              df_append_i$latitude[2]))
    
    return(df_append_i)
  })
  
  return(df_c_append)
  
})

# Subset -----------------------------------------------------------------------
#### Restrict pairs to within distance 
df_pairs <- df_pairs %>%
  dplyr::filter(dist_uids_m <= 10000)

#### If uid matched with multiple, keep closest
df_pairs <- df_pairs %>%
  arrange(dist_uids_m) %>%
  distinct(uid_t2, year, .keep_all = T)

# Difference -------------------------------------------------------------------
df_change <- df_pairs %>%
  as.data.frame() %>%
  #dplyr::mutate(year_str = year %>% as.character()) %>%
  arrange(year) %>%
  group_by(uid_panel, country_code, country_name, gadm_uid, within_country_fold, continent_adj, iso2,
           urban_rural_yr1, urban_rural_yr2) %>%
  summarise_if(is.numeric, diff) %>%
  ungroup() %>%
  dplyr::rename(year_diff = year) %>%
  dplyr::rename(uid = uid_panel)
#dplyr::rename(year_diff = year,
#              year      = year_str) %>%
#dplyr::filter(!is.na(year_diff))


# Export -----------------------------------------------------------------------
saveRDS(df_change, file.path(data_dir, SURVEY_NAME, "FinalData", "Merged Datasets", "survey_alldata_clean_changes_cluster.Rds"))

