# Clean survey data

# Load Data --------------------------------------------------------------------
df <- readRDS(file.path(data_dir, SURVEY_NAME, "FinalData", "Merged Datasets", "survey_alldata.Rds"))

# Add Continent/Region ---------------------------------------------------------
df <- df %>%
  mutate(continent = iso2 %>% countrycode(origin = "iso2c", destination = "continent"),
         un_region = iso2 %>% countrycode(origin = "iso2c", destination = "un.region.name"),
         un_subregion = iso2 %>% countrycode(origin = "iso2c", destination = "un.regionsub.name")) %>%
  mutate(continent_adj = case_when(
    continent %in% c("Asia", "Europe", "Oceania") ~ "Eurasia",
    TRUE ~ continent
  ))

# Remove Guyana (no OSM data)
df <- df %>%
  dplyr::filter(country_code != "GY")

# Remove missing
#df <- df %>%
#  dplyr::filter(!is.na(fb_estimate_mau_1))

# Facebook - Cleanup -----------------------------------------------------------
# For all Facebook variables except total users, if 1000, make 0

if_1000_make_0 <- function(x){
  x[x %in% 1000] <- 0
  return(x)
}

df <- df %>%
  dplyr::mutate_at(vars(contains("mau"), -fb_estimate_mau_upper_bound_1), if_1000_make_0)

# Facebook - Proportion --------------------------------------------------------
truncate_1 <- function(x){
  x[x > 1] <- 1
  return(x)
}

df_fb_prop <- df %>%
  dplyr::mutate_at(vars(contains("mau"), -fb_estimate_mau_upper_bound_1), ~(. / fb_estimate_mau_upper_bound_1)) %>%
  dplyr::select(-c(fb_estimate_mau_upper_bound_1)) %>%
  dplyr::select(uid, contains("fb_")) %>%
  dplyr::mutate_at(vars(contains("fb_")), truncate_1) %>%
  rename_at(vars(-uid), ~ str_replace_all(., "fb_", "fb_prop_")) 

df <- df %>%
  left_join(df_fb_prop, by = "uid")

# Facebook - Divide by World Pop -----------------------------------------------
# Sometimes WP is zero but have a value for facebook, where [value]/0 turns to Inf

# TODO: Update with radius; 2, 5, 10 -- for Facebook

inf_to_zero <- function(x){
  x[x == Inf] <- 0
  return(x)
}

df_fb_wp <- df %>%
  dplyr::mutate_at(vars(contains("fb_estimate_")), ~ . / worldpop_10km) %>%
  dplyr::select_at(vars(uid, year, contains("fb_estimate_"))) %>%
  rename_at(vars(-uid, -year), ~ str_replace_all(., "fb_estimate", "fb_wp_estimate")) %>%
  dplyr::mutate_at(vars(contains("fb_wp")), inf_to_zero) %>%
  dplyr::mutate_at(vars(contains("fb_wp")), truncate_1) 

df <- df %>%
  left_join(df_fb_wp, by = c("uid", "year"))

# Log Values -------------------------------------------------------------------
log_p1 <- function(x){
  log(x + 1)
}

df <- df %>%
  dplyr::mutate_at(vars(contains("osm_dist")), log_p1)

df$viirs_avg_rad <- log(df$viirs_avg_rad+1)

# Remove Variables -------------------------------------------------------------
#df <- df %>%
  
  ## Facebook daily active users
#  dplyr::select_at(vars(-contains("_dau_")))

# Remove Observations ----------------------------------------------------------

df$cnn_s2_rgb_pc1 %>% is.na %>% table()

df <- df %>%
  dplyr::filter(!is.na(worldclim_bio_1),
                !is.na(fb_estimate_mau_upper_bound_2),
                !is.na(globalmod_mean),
                !is.na(l8_B1),
                !is.na(cnn_s2_rgb_pc1))

nrow(df)

# Export Data ------------------------------------------------------------------
saveRDS(df, file.path(data_dir, SURVEY_NAME, "FinalData", "Merged Datasets", "survey_alldata_clean.Rds"))
#write.csv(df, file.path(data_dir, SURVEY_NAME, "FinalData", "Merged Datasets", "survey_alldata_clean.csv"),
#          row.names = F)

## Check NAs
df_sum <- df %>%
  dplyr::filter(country_code != "GY") %>%
  dplyr::select_if(is.numeric) %>%
  mutate(id = 1:n()) %>%
  pivot_longer(cols = -id) %>%
  group_by(name) %>%
  summarise_at(vars(value), .funs = list(N_NA = ~ sum(is.na(.)),
                                         mean = ~ mean(., na.rm = T),
                                         min = ~ min(., na.rm = T),
                                         max = ~ max(., na.rm = T)))

df_sum_na <- df_sum %>%
  dplyr::filter(N_NA > 0)

