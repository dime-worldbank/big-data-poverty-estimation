# Determinants of Model Performance

# Load data --------------------------------------------------------------------
# TODO: Merge in country-level data (eg, WDI, Facebook penetration, etc)

results_df <- readRDS(file.path(data_dir, SURVEY_NAME, "FinalData", "pov_estimation_results",
                                "accuracy_appended.Rds"))

wdi_df <- readRDS(file.path(data_dir, "WDI", "FinalData", "wdi.Rds"))

fb_wide_df <- readRDS(file.path(fb_marketing_dir,  "FinalData", "country_level_mau", 
                                "Individual Datasets",
                                "country_level_mau.Rds"))

# Merge data -------------------------------------------------------------------
#### Prep data for merging
wdi_df <- wdi_df %>%
  dplyr::select(-c(iso3c, country, year, capital, longitude, latitude))

fb_wide_df <- fb_wide_df %>%
  dplyr::rename(iso2 = country_iso2)

#### Merge
results_df <- results_df %>%
  left_join(wdi_df, by = "iso2") %>%
  left_join(fb_wide_df, by = "iso2")

# Analysis ---------------------------------------------------------------------





