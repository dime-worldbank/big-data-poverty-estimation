# Determinants of Model Performance

# TO PREDICT ON
# 1. Can also predict on continent (simple model)
# 2. Survey date

# Load data --------------------------------------------------------------------
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

# Construct variables ----------------------------------------------------------
results_df <- results_df %>%
  dplyr::mutate(prop_pop_on_fb = estimate_mau_1 / wdi_population)

# Analysis ---------------------------------------------------------------------
results_sum_df <- results_df %>%
  dplyr::filter(feature_type %in% "fb_prop",
                estimation_type %in% "within_country_cv",
                target_var %in% "pca_allvars")
results_sum_df %>%
  ggplot(aes(x = r2,
             prop_pop_on_fb)) +
  geom_point()


results_df %>%
  dplyr::filter(feature_type %in% "all",
                estimation_type %in% "best",
                target_var %in% "pca_allvars")


results_sum_df$income

