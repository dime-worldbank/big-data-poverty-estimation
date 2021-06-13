# Merge Data Extracted from Facebook API

# Load Data --------------------------------------------------------------------
#opm_cluster_crosswalk <- readRDS(file.path(project_file_path, "Data", SURVEY_NAME, "FinalData", "Individual Datasets", "fb_mau_cluster_crosswalk.Rds"))

# To Wide ----------------------------------------------------------------------
df_long <- file.path(project_file_path, "Data", SURVEY_NAME, "FinalData", "Individual Datasets", 
                     "fb_mau_individual_datasets") %>%
  list.files(pattern = "*.Rds",
             full.names = T) %>%
  map_df(readRDS)

df_long <- df_long %>%
  dplyr::rename(uid = cluster_id)

df_wide <- df_long %>%
  pivot_wider(id_cols = c(uid),
              names_from = param_id,
              values_from = c(estimate_dau, estimate_mau))

# To Proportion ----------------------------------------------------------------
# estimate_[mau/dau]_1 is all facebook users, so divide by this to 
# get proportion of facebook users that meet a certain category

df_wide_prop <- df_wide %>%
  mutate_at(vars(contains("dau"), -estimate_dau_1), ~(. /  estimate_dau_1)) %>%
  mutate_at(vars(contains("mau"), -estimate_mau_1), ~(. /  estimate_mau_1)) %>%
  dplyr::select(-c(estimate_dau_1, estimate_mau_1)) %>%
  dplyr::mutate_all(replace_na, 0)

# Export -----------------------------------------------------------------------
saveRDS(df_wide, file.path(data_dir, SURVEY_NAME, "FinalData", "Individual Datasets", "facebook_marketing_dau_mau.Rds"))
write.csv(df_wide, file.path(data_dir, SURVEY_NAME, "FinalData", "Individual Datasets", "facebook_marketing_dau_mau.csv"), row.names = F)

saveRDS(df_wide_prop, file.path(data_dir, SURVEY_NAME, "FinalData", "Individual Datasets", "facebook_marketing_dau_mau_prop.Rds"))
write.csv(df_wide_prop, file.path(data_dir, SURVEY_NAME, "FinalData", "Individual Datasets", "facebook_marketing_dau_mau_prop.csv"), row.names = F)

# Indicatoes parameters for each "param_version"
param_df <- df_long %>%
  dplyr::select(-c(estimate_dau, estimate_mau, estimate_ready, latitude, longitude, api_call_time_utc)) %>%
  dplyr::distinct(param_id, .keep_all = T) %>%
  dplyr::select(param_id, everything())

saveRDS(param_df, file.path(data_dir, SURVEY_NAME, "FinalData", "Individual Datasets", "facebook_marketing_parameters.Rds"))
write.csv(param_df, file.path(data_dir, SURVEY_NAME, "FinalData", "Individual Datasets", "facebook_marketing_parameters.csv"), row.names = F)









