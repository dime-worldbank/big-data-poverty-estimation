# Clean survey data

# Load Data --------------------------------------------------------------------
df <- readRDS(file.path(data_dir, "DHS", "FinalData", "Merged Datasets", "survey_alldata.Rds"))

# Add Continent/Region ---------------------------------------------------------
df <- df %>%
  mutate(continent = iso2 %>% countrycode(origin = "iso2c", destination = "continent"),
         un_region = iso2 %>% countrycode(origin = "iso2c", destination = "un.region.name"),
         un_subregion = iso2 %>% countrycode(origin = "iso2c", destination = "un.regionsub.name")) %>%
  mutate(continent_adj = case_when(
    continent %in% c("Asia", "Europe", "Oceania") ~ "Eurasia",
    TRUE ~ continent
  ))

# Facebook - Cleanup -----------------------------------------------------------
# For all Facebook variables except total users, if 1000, make 0

if_1000_make_0 <- function(x){
  x[x %in% 1000] <- 0
  return(x)
}

df$fb_estimate_mau_upper_bound_1[is.na(df$fb_estimate_mau_upper_bound_1)] <- 0

df <- df %>%
  dplyr::mutate_at(vars(contains("mau"), -fb_estimate_mau_upper_bound_1), if_1000_make_0)

# Facebook - Proportion --------------------------------------------------------
# Create variables of the proportion of Facebook users within each category
truncate_1 <- function(x){
  x[x > 1] <- 1
  return(x)
}

df_fb_prop <- df %>%
  dplyr::mutate_at(vars(contains("mau"), -fb_estimate_mau_upper_bound_1), ~(. / fb_estimate_mau_upper_bound_1)) %>%
  dplyr::select(-c(fb_estimate_mau_upper_bound_1)) %>%
  dplyr::select(uid, contains("fb_"), -contains("fb_rwi")) %>%
  dplyr::mutate_at(vars(contains("fb_")), truncate_1) %>%
  rename_at(vars(-uid), ~ str_replace_all(., "fb_", "fb_prop_")) 

df <- df %>%
  left_join(df_fb_prop, by = "uid")

# Facebook - Divide by World Pop -----------------------------------------------
# Calculate proportion of population is on Facebook

# Sometimes WP is zero but have a value for facebook, where [value]/0 turns to Inf
inf_to_zero <- function(x){
  x[x == Inf] <- 0
  return(x)
}

df_fb_wp <- df %>% 
  ## Divide by World Pop population, depending on Facebook radius used
  mutate(across(contains('fb_estimate_'), 
                ~ case_when(fb_radius %in% 2 ~ . / worldpop_2km, 
                            fb_radius %in% 5 ~ . / worldpop_5km, 
                            fb_radius %in% 10 ~ . / worldpop_10km))) %>%
  
  ## Select specific variables and rename
  dplyr::select(uid, contains("fb_estimate_")) %>%
  rename_at(vars(-uid), ~ str_replace_all(., "fb_estimate", "fb_wp_prop_estimate")) %>%
  
  ## Cleanup values
  dplyr::mutate_at(vars(contains("fb_wp")), inf_to_zero) %>%
  dplyr::mutate_at(vars(contains("fb_wp")), truncate_1) %>%
  
  ## Only keep % of population on Facebook
  dplyr::select(uid, fb_wp_prop_estimate_mau_upper_bound_1)

df <- df %>%
  left_join(df_fb_wp, by = c("uid"))

# Log Values -------------------------------------------------------------------
log_p1 <- function(x){
  log(x + 1)
}

df <- df %>%
  dplyr::mutate_at(vars(contains("osm_dist")), log_p1)

df$viirs_avg_rad         <- log(df$viirs_avg_rad + 1)
df$viirs_avg_rad_sdspace <- log(df$viirs_avg_rad_sdspace + 1)
df$viirs_avg_rad_sdtime  <- log(df$viirs_avg_rad_sdtime + 1)

# Remove Variables -------------------------------------------------------------
df <- df %>%
  dplyr::select_at(vars(
    # ID/Information variables
    "uid", "year", "iso2", "country_code", "country_name", 
    "continent_adj", "within_country_fold", "urban_rural",
    "latitude", "longitude", "gadm_uid", "GID_1", "GID_2",
    "most_recent_survey",
    
    "n_hh_members", "n_hh_members_sum",
    
    # DHS variables to compare FB data with
    "educ_years_hh_max",
    starts_with("educ_levels"),
    
    # Target variables
    "pca_allvars",
    "pca_allvars_mr",
    "pca_allvars_noroof_mr",
    "pca_physicalvars_mr",
    "pca_physicalvars_noroof_mr",
    "pca_nonphysicalvars_mr",
    # "pca_allvars_rmna_mr",
    # "pca_allvars_noroof_rmna_mr",
    # "pca_physicalvars_rmna_mr",
    # "pca_physicalvars_noroof_rmna_mr",
    # "pca_nonphysicalvars_rmna_mr",
    "wealth_index_score",
    "wealth_index",
    
    # Standard Deviation
    "pca_allvars_stddev",
    "pca_allvars_mr_stddev",
    
    # Features
    starts_with("viirs_"),
    
    starts_with("ntlharmon_"),
    
    starts_with("cnn_"),
    
    starts_with("s1_sar_"),
    
    starts_with("mosaik_"),
    
    starts_with("l8_"),
    starts_with("l7_"),
    
    starts_with("fb_prop_"),
    starts_with("fb_wp_prop"),
    
    starts_with("osm_"),
    
    starts_with("gc_"),
    starts_with("elevslope_"),
    
    starts_with("weather_"),
    starts_with("worldclim_"),
    
    starts_with("pollution_"))
  ) %>%
  dplyr::select(-c(fb_prop_radius))

# Remove Observations/Variables ------------------------------------------------

df <- df %>%
  dplyr::filter(!is.na(s1_sar_vh_mean))

df <- df %>%
  dplyr::select(-c(pca_allvars_noroof_mr,
                   pca_physicalvars_mr,
                   pca_physicalvars_noroof_mr,
                   pca_nonphysicalvars_mr))

# Export Data ------------------------------------------------------------------
df$fb_prop_radius <- NULL
saveRDS(df, file.path(data_dir, "DHS", "FinalData", "Merged Datasets", "survey_alldata_clean.Rds"))






