# Append Globcover Data

## Load data
ntl_all_df <- file.path(data_dir, SURVEY_NAME, "FinalData", "Individual Datasets", "ntl_harmonized") %>%
  list.files(pattern = "ntlharmon_", full.names = T) %>%
  str_subset(paste0(BUFFER_SATELLITE,"m")) %>%
  map_df(readRDS)

## Export data
saveRDS(ntl_all_df, file.path(data_dir, SURVEY_NAME, "FinalData", "Individual Datasets", "ntl_harmonized.Rds"))

