# Table of Survey Year for Each Country

# Load data --------------------------------------------------------------------
df <- readRDS(file.path(dhs_dir, "FinalData", "Individual Datasets", "survey_socioeconomic.Rds"))

# Summarize data ---------------------------------------------------------------
df_sum <- df %>%
  group_by(country_name) %>%
  dplyr::summarise(year_min = min(year),
                   year_max = max(year),
                   n = n())

df_sum$year_min[df_sum$year_min == df_sum$year_max] <- ""



