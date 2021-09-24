# Append Globcover Data

## Load data
wc_all_df <- file.path(data_dir, SURVEY_NAME, "FinalData", "Individual Datasets", "worldclim") %>%
  list.files(pattern = "worldclim_", full.names = T) %>%
  str_subset("2500m") %>%
  map_df(readRDS)

## Export data
saveRDS(wc_all_df, file.path(data_dir, SURVEY_NAME, "FinalData", "Individual Datasets", "worldclim.Rds"))
