# N Folds Per Country

# Load Data --------------------------------------------------------------------
survey_df <- readRDS(file.path(dhs_dir, "FinalData", "Individual Datasets", "survey_socioeconomic.Rds"))

fold_df <- survey_df %>%
  distinct(country_code, within_country_fold) %>%
  group_by(country_code) %>%
  dplyr::summarise(N = n())
  
fold_df %>%
  filter(N != 5)
