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
  dplyr::mutate(estimation_type = case_when(
    estimation_type %>% str_detect("continent_") & estimation_type %>% str_detect("country_pred") ~ "Same Continent",
    estimation_type %in% "global_country_pred" ~ "Global",
    estimation_type %in% "within_country_cv" ~ "Within Country",
    estimation_type %in% "continent" ~ "Other Continents"
  ))

# Merge with select survey variables -------------------------------------------
survey_df <- readRDS(file.path(data_dir, SURVEY_NAME, "FinalData", "Merged Datasets", "survey_alldata_clean.Rds"))

survey_df <- survey_df %>%
  distinct(country_code, continent_adj) %>%
  dplyr::rename(country = country_code)

acc_df <- acc_df %>%
  dplyr::left_join(survey_df, by = "country")

# Export data ------------------------------------------------------------------
saveRDS(acc_df, 
        file.path(data_dir, SURVEY_NAME, "FinalData", "pov_estimation_results",
                  "accuracy_appended.Rds"))

