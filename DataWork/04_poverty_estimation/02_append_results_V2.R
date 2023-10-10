
df <- file.path(data_dir, "DHS", "FinalData", "pov_estimation_results_v2",
          "prediction") %>% 
  list.files(full.names = T, pattern = "*.Rds") %>%
  str_subset("prediction_") %>%
  #str_subset("global_country_pred") %>%
  str_subset("pca_allvars") %>%
  map_df(readRDS)

sum_df <- df %>%
  group_by(country_code, country_name, estimation_type, feature_type, target_var_dep, level_change) %>%
  dplyr::summarise(r2 = cor(target_var, prediction)^2,
                   R2 = R2(target_var, prediction, form = "traditional")) %>%
  ungroup()

sum_df %>%
  dplyr::filter(level_change == "levels",
                feature_type == "all") %>%
  ggplot() +
  geom_boxplot(aes(x = r2,
                   y = estimation_type))

df_a <- df %>%
  dplyr::filter(level_change == "changes",
                feature_type == "all_changes") 
cor(df_a$target_var, df_a$prediction)^2
R2(df_a$target_var, df_a$prediction, form = "traditional")

df %>%
  dplyr::filter(level_change == "changes",
                feature_type == "all_changes") %>%
  ggplot() +
  geom_point(aes(x = target_var, y = prediction))

sum_df %>%
  dplyr::filter(level_change == "levels",
                feature_type == "all") %>%
  pull(R2) %>%
  summary()

sum_df$feature_type %>% table()

sum_df %>%
  ggplot() +
  geom_boxplot(aes(x = r2,
                   y = feature_type)) +
  facet_wrap(~level_change,
             scales = "free")
