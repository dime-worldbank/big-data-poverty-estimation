# Main Results

# Load data --------------------------------------------------------------------
results_df <- readRDS(file.path(data_dir, SURVEY_NAME, "FinalData", "pov_estimation_results",
                                "accuracy_appended.Rds"))

# 1. All features, best estimation type ----------------------------------------
results_df %>%
  dplyr::filter(feature_type %in% "all",
                target_var %in% "pca_allvars") %>%
  ggplot() +
  geom_boxplot(aes(y = reorder(estimation_type_clean, r2, FUN = median, .desc =TRUE),
                   x = r2))

# 2. By feature ----------------------------------------------------------------
results_df %>%
  dplyr::filter(target_var %in% "pca_allvars",
                estimation_type %in% "within_country_cv") %>%
  ggplot() +
  geom_boxplot(aes(y = reorder(feature_type_clean, r2, FUN = median, .desc =TRUE),
                   x = r2))

# 3. By target variable and feature type ---------------------------------------
results_df %>%
  dplyr::filter(estimation_type %in% "within_country_cv",
                target_var %in% "wealth_index_score") %>%
  ggplot() +
  geom_boxplot(aes(y = reorder(feature_type_clean, r2, FUN = median, .desc =TRUE),
                   x = r2)) +
  labs(y = "Features used for poverty estimation")

# 4. By target variable and feature type ---------------------------------------
results_df %>%
  dplyr::filter(estimation_type %in% "within_country_cv") %>%
  ggplot() +
  geom_boxplot(aes(y = reorder(feature_type_clean, r2, FUN = median, .desc =TRUE),
                   x = r2,
                   fill = target_var_clean)) +
  labs(y = "Features used for poverty estimation")

# 5. All results ---------------------------------------------------------------
results_df %>%
  ggplot() +
  geom_boxplot(aes(y = feature_type_clean,
                   x = cor,
                   #group = estimation_type,
                   fill = estimation_type_clean)) +
  facet_wrap(~target_var_clean)

# 6. For each country, which is the best estimation type? ----------------------
# TODO: Second best? (within_country is 1st OR 2nd best?, then average difference 
# in these cases)

x_highest_num <- function(values, xth_highest_num){
  # Return second highest number (instead of max)
  # ARGs
  # -- values: vector of values
  # -- xth_highest_num: Which highest number to return (e.g., 2 = return 2nd highest)
  return(sort(values, decreasing = T)[xth_highest_num])
}

best_est_df <- results_df %>%
  dplyr::filter(target_var %in% "pca_allvars",
                feature_type %in% "all") %>%
  
  # Variable of best estimation type
  group_by(country_name) %>%
  dplyr::mutate(estimation_type_rank1 = estimation_type_clean[r2 == max(r2)][1]) %>%
  dplyr::mutate(estimation_type_rank2 = estimation_type_clean[r2 == x_highest_num(r2, 2)][1]) %>%
  dplyr::mutate(estimation_type_rank3 = estimation_type_clean[r2 == x_highest_num(r2, 3)][1]) %>%
  dplyr::mutate(estimation_type_rank4 = estimation_type_clean[r2 == x_highest_num(r2, 4)][1]) %>%
  ungroup() %>%
  
  # Pivot so one country per row
  pivot_wider(id_cols = c(continent_adj, country_name, 
                          estimation_type_rank1, estimation_type_rank2,
                          estimation_type_rank3, estimation_type_rank4),
                     names_from = estimation_type,
                     values_from = r2)

## Overall
best_est_df %>%
  group_by(estimation_type_rank1) %>%
  dplyr::summarise(N = n()) %>%
  ungroup() %>%
  dplyr::mutate(prop = N / sum(N)) %>%
  arrange(-N)

## By continent
best_est_df$continent_adj %>% table()

best_est_df %>%
  group_by(estimation_type_rank1, continent_adj) %>%
  dplyr::summarise(N = n()) %>%
  ungroup() %>%
  group_by(continent_adj) %>% # Proportion within the continent
  dplyr::mutate(prop = N / sum(N)) %>%
  arrange(continent_adj, -N)




