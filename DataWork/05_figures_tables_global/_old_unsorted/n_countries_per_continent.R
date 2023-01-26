# N Countries per Continent

survey_df <- readRDS(file.path(dhs_dir, "FinalData", "Merged Datasets", "survey_alldata_clean.Rds"))

survey_df %>%
  distinct(continent, country_code) %>%
  group_by(continent) %>%
  dplyr::summarise(N = n())
