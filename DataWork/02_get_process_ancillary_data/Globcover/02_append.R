# Append Globcover Data

## Load data
gc_all_df <- file.path(data_dir, SURVEY_NAME, "FinalData", "Individual Datasets", "globcover") %>%
  list.files(pattern = "gc_", full.names = T) %>%
  str_subset("2500m") %>%
  map_df(readRDS)

## Export data
saveRDS(gc_all_df, file.path(data_dir, SURVEY_NAME, "FinalData", "Individual Datasets", "globcover.Rds"))
