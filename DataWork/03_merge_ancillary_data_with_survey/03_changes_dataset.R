# Make Dataset of Changes

# Changes at ADM level, first difference

# Load data --------------------------------------------------------------------
df <- readRDS(file.path(data_dir, SURVEY_NAME, "FinalData", "Merged Datasets", "survey_alldata_clean.Rds"))

# Select variables and aggregate to ADM ----------------------------------------
# Select relevant ID variables and variables that change over time

df_agg <- df %>%
  dplyr::filter(year >= 2000) %>%
  dplyr::select(country_code, gadm_uid, iso2, year, within_country_fold, continent_adj, 
                pca_allvars, wealth_index_score,
                contains("l7_"),
                contains("gc_"),
                contains("ntlharmon_"),
                contains("weather_"),
                contains("pollution_aod_")) %>%
  group_by(country_code, gadm_uid, iso2, year, within_country_fold, continent_adj) %>%
  summarise_if(is.numeric, ~mean(., na.rm = T)) %>%
  ungroup() 

# Difference -------------------------------------------------------------------
# Do all difference combinations using different lag amounts.
max_lag <- df_agg %>%
  distinct(country_code, year) %>%
  group_by(country_code) %>%
  dplyr::summarise(n = n()) %>%
  pull(n) %>%
  max()

lag_x <- function(x, n){
  # x ~ Value
  # n ~ Lag amount (1 lag, 2 lag, etc)
  x - lag(x, n)
}

df_agg_diff <- map_df(1:max_lag, function(i){
  print(i)
  df_agg %>%
    dplyr::mutate(year_str = year %>% as.character()) %>%
    arrange(year) %>%
    group_by(country_code, gadm_uid, within_country_fold, continent_adj, iso2) %>%
    mutate_if(is.numeric, ~lag_x(., i)) %>%
    ungroup() %>%
    dplyr::rename(year_diff = year,
                  year      = year_str) %>%
    dplyr::mutate(lag = i) %>%
    dplyr::filter(!is.na(year_diff))
})

## For each country, variable flagging largest year_diff
df_agg_diff <- df_agg_diff %>%
  group_by(country_code) %>%
  dplyr::mutate(year_diff_max = year_diff == max(year_diff)) %>%
  ungroup()

## Make complete
df_agg_diff <- df_agg_diff %>%
  dplyr::filter(!is.na(ntlharmon_avg),
                !is.na(l7_B1),
                !is.na(gc_190),
                !is.na(weather_all_maximum_2m_air_temperature),
                !is.na(pollution_aod_047))

## Number of observations within each country
df_agg_diff <- df_agg_diff %>%
  group_by(year, country_code) %>%
  dplyr::mutate(n_obs = n()) %>%
  ungroup()

## Make uid
df_agg_diff <- df_agg_diff %>%
  dplyr::mutate(uid = paste(country_code, gadm_uid, year, lag))

# Export -----------------------------------------------------------------------
saveRDS(df_agg_diff, file.path(data_dir, SURVEY_NAME, "FinalData", "Merged Datasets", "survey_alldata_clean_changes.Rds"))


