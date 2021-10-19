# Append Country Level Results

fb_df <- file.path(fb_marketing_dir,  "FinalData", "country_level_mau", "Individual Datasets") %>%
  list.files(full.names = T,
             pattern = "*.Rds") %>%
  map_df(readRDS)

param_sum_df <- fb_df %>%
  group_by(param_id) %>%
  dplyr::summarise(N_above_1000 = sum(estimate_mau > 1000),
                   mau_median = median(estimate_mau),
                   mau_max = max(estimate_mau))


fb_wide_df <- fb_df %>%
  pivot_wider(id_cols = c(country_iso2),
              names_from = param_id,
              values_from = c(estimate_dau, estimate_mau))

fb_wide_df <- fb_wide_df %>%
  dplyr::select(country_iso2, contains("mau"))

saveRDS(fb_wide_df, file.path(fb_marketing_dir,  "FinalData", "country_level_mau", 
                              "Individual Datasets",
                              "country_level_mau.Rds"))