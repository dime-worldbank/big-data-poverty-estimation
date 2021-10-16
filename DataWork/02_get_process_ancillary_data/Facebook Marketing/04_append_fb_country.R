# Append Country Level Results

fb_df <- file.path(fb_marketing_dir,  "FinalData", "country_level_mau", "Individual Datasets") %>%
  list.files(full.names = T,
             pattern = "*.Rds") %>%
  map_df(readRDS)

fb_wide_df <- fb_df %>%
  pivot_wider(id_cols = c(country_iso2),
              names_from = param_id,
              values_from = c(estimate_dau, estimate_mau))
