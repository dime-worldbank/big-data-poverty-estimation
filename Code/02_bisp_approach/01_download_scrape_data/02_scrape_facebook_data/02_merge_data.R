# Merge Data Extracted from Facebook API

# Load Data --------------------------------------------------------------------
uid_clusterid_crosswalk <- readRDS(file.path(final_data_file_path, "BISP", "Individual Datasets", "facebook_marketing_extract_clusterid_crosswalk.Rds"))

facebook_1_df <- readRDS(file.path(final_data_file_path, "BISP", "Individual Datasets", "facebook_marketing_extract_1.Rds"))
facebook_2_df <- readRDS(file.path(final_data_file_path, "BISP", "Individual Datasets", "facebook_marketing_extract_2.Rds"))
facebook_3_df <- readRDS(file.path(final_data_file_path, "BISP", "Individual Datasets", "facebook_marketing_extract_3.Rds"))

facebook_1_df <- facebook_1_df %>%
  dplyr::select(cluster_id, estimate_dau, estimate_mau) %>%
  dplyr::rename(estimate_dau_all = estimate_dau,
                estimate_mau_all = estimate_mau)

facebook_2_df <- facebook_2_df %>%
  dplyr::select(cluster_id, estimate_dau, estimate_mau) %>%
  dplyr::rename(estimate_dau_male = estimate_dau,
                estimate_mau_male = estimate_mau)

facebook_3_df <- facebook_3_df %>%
  dplyr::select(cluster_id, estimate_dau, estimate_mau) %>%
  dplyr::rename(estimate_dau_female = estimate_dau,
                estimate_mau_female = estimate_mau)

facebook_df <- merge(facebook_1_df, facebook_2_df, by="cluster_id", all=T)
facebook_df <- merge(facebook_df, facebook_3_df, by="cluster_id", all=T)
hh_facebook_df <- merge(uid_clusterid_crosswalk, facebook_df, by="cluster_id")

# Export -----------------------------------------------------------------------
hh_facebook_df$cluster_id <- NULL
saveRDS(hh_facebook_df, file.path(final_data_file_path, "BISP", "Individual Datasets", "facebook_marketing_extract_allmerged.Rds"))













