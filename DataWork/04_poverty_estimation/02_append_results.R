# Append Poverty Estimation Results

# Load/append data -------------------------------------------------------------
acc_df <- file.path(data_dir, SURVEY_NAME, "FinalData", "pov_estimation_results", "accuracy") %>%
  list.files(pattern = "*.Rds",
             full.names = T) %>%
  map_df(readRDS)

# Clean ------------------------------------------------------------------------
acc_df <- acc_df %>%
  # For continent, predict across all countries; but we care about aggregating 
  # to countries
  dplyr::mutate(cor_country = case_when(
    estimation_type == "continent" ~ cor_fold,
    TRUE ~ cor_all
  )) %>%
  dplyr::mutate(country_code = country_code %>% as.character()) %>%
  dplyr::mutate(country = case_when(
    estimation_type %in% "continent" ~ country_code,
    TRUE ~ country
  )) %>%
  dplyr::mutate(estimation_type_clean = case_when(
    estimation_type %>% str_detect("continent_") & estimation_type %>% str_detect("country_pred") ~ "Same Continent",
    estimation_type %in% "global_country_pred" ~ "Global",
    estimation_type %in% "within_country_cv" ~ "Within Country",
    estimation_type %in% "continent" ~ "Other Continents"
  )) %>%
  dplyr::mutate(estimation_type = case_when(
    estimation_type %>% str_detect("continent_") & estimation_type %>% str_detect("country_pred") ~ "same_continent",
    estimation_type %in% "continent" ~ "other_continents",
    TRUE ~ estimation_type
  )) %>%
  dplyr::mutate(feature_type_clean = case_when(
    feature_type == "all" ~ "All Features",
    feature_type == "cnn_s2_bu" ~ "CNN: Built-Up Index",
    feature_type == "cnn_s2_ndvi" ~ "CNN: NDVI",
    feature_type == "cnn_s2_rgb" ~ "CNN: RGB",
    feature_type == "fb_prop" ~ "Facebook: Proportion",
    feature_type == "gc" ~ "Landcover - GlobCover",
    feature_type == "l8" ~ "Daytime Imagery - Average",
    feature_type == "osm" ~ "OpenStreetMap",
    feature_type == "pollution" ~ "Pollution",
    #feature_type == "" ~ "",
    TRUE ~ feature_type
  )) %>%
  dplyr::mutate(target_var_clean = case_when(
    target_var == "pca_allvars" ~ "Asset Index",
    target_var == "pca_nonphysicalvars" ~ "Asset Index: Non-Physical Assets",
    target_var == "pca_physicalvars" ~ "Asset Index: Physical Assets",
    target_var == "wealth_index_score" ~ "DHS Wealth Index",
    TRUE ~ target_var
  ))

# Merge with select survey variables -------------------------------------------
survey_df <- readRDS(file.path(data_dir, SURVEY_NAME, "FinalData", "Merged Datasets", "survey_alldata_clean.Rds"))

survey_df <- survey_df %>%
  distinct(country_code, country_name, continent_adj, iso2) %>%
  dplyr::rename(country = country_code)

acc_df <- acc_df %>%
  dplyr::left_join(survey_df, by = "country")

# Aggregate Across Folds -------------------------------------------------------
# Notes:
# - cor_fold only different than cor_country for within_country estimation
# - cor_all only different than cor_country for other continents estimation

acc_all_df <- acc_df %>%
  group_by(estimation_type, estimation_type_clean,
           feature_type, feature_type_clean,
           target_var, target_var_clean, 
           country, country_name, continent_adj, iso2) %>%
  dplyr::summarise(N = sum(N_fold),
                   cor = cor_country[1]) %>% # This repeats across folds
  dplyr::mutate(r2 = cor^2)

# Add rows for best estimation type --------------------------------------------
acc_all_best_df <- acc_all_df %>%
  group_by(country, target_var, feature_type, feature_type_clean) %>%
  slice_max(order_by = r2, n = 1) %>%
  mutate(estimation_type_clean = "Using Best Training Sample\nType for Each Country",
         estimation_type = "best") %>%
  mutate(est_cat = "Best")

acc_all_df <- bind_rows(
  acc_all_df,
  acc_all_best_df
)

# Export data ------------------------------------------------------------------
saveRDS(acc_all_df, 
        file.path(data_dir, SURVEY_NAME, "FinalData", "pov_estimation_results",
                  "accuracy_appended.Rds"))


