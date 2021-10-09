# Clean survey data

# Load Data --------------------------------------------------------------------
df <- readRDS(file.path(data_dir, SURVEY_NAME, "FinalData", "Merged Datasets", "survey_alldata.Rds"))

# Add Continent/Region ---------------------------------------------------------
df <- df %>%
  dplyr::mutate(iso2 = case_when(
    country_name == "Dominican Republic" ~ "DO",
    country_name == "India" ~ "IN",
    country_name == "Burundi" ~ "BI",
    country_name == "Namibia" ~ "NA",
    TRUE ~ country_code
  )) %>%
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
df <- df %>%
  dplyr::filter(!is.na(fb_estimate_mau_1))

# Facebook - Divide by World Pop -----------------------------------------------
df_fb_wp <- df %>%
  dplyr::mutate_at(vars(contains("fb_estimate_")), ~ . / worldpop_10km) %>%
  dplyr::select_at(vars(uid, year, contains("fb_estimate_"))) %>%
  rename_at(vars(-uid, -year), ~ str_replace_all(., "fb_estimate", "fb_wp_estimate"))

df <- df %>%
  left_join(df_fb_wp, by = c("uid", "year"))

# Log Values -------------------------------------------------------------------
log_p1 <- function(x){
  log(x + 1)
}

df <- df %>%
  dplyr::mutate_at(vars(contains("osm_dist")), log_p1)

# Remove Variables -------------------------------------------------------------
df <- df %>%
  
  ## Facebook daily active users
  dplyr::select_at(vars(-contains("_dau_")))

# Export Data ------------------------------------------------------------------
saveRDS(df, file.path(data_dir, SURVEY_NAME, "FinalData", "Merged Datasets", "survey_alldata_clean.Rds"))
write.csv(df, file.path(data_dir, SURVEY_NAME, "FinalData", "Merged Datasets", "survey_alldata_clean.csv"),
          row.names = F)

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

