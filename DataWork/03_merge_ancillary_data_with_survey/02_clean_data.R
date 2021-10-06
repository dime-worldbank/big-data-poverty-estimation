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

# Export Data ------------------------------------------------------------------
saveRDS(df, file.path(data_dir, SURVEY_NAME, "FinalData", "Merged Datasets", "survey_alldata_clean.Rds"))
write.csv(df, file.path(data_dir, SURVEY_NAME, "FinalData", "Merged Datasets", "survey_alldata_clean.csv"),
          row.names = F)

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

