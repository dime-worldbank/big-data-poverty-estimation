# Compare Wealth Indices

df <- readRDS(file.path(data_dir, SURVEY_NAME, "FinalData", "Merged Datasets", "survey_alldata_clean.Rds"))

cor.test(df$pca_allvars, df$pca_allvars_mr)

df %>%
  ggplot() +
  geom_point(aes(x = pca_allvars,
                 y = pca_allvars_mr))

df_sum <- df %>%
  filter(most_recent_survey %in% T) %>%
  group_by(country_code) %>%
  dplyr::summarise(cor_pca_allvars = cor(pca_allvars,      wealth_index_score),
                   cor_pca_allvars_mr = cor(pca_allvars_mr, wealth_index_score)) %>%
  ungroup()

summary(df_sum$cor_pca_allvars)
summary(df_sum$cor_pca_allvars_mr)




df %>%
  filter(most_recent_survey %in% T) %>%
  group_by(country_code) %>%
  dplyr::summarise(cor_pca_allvars = cor(pca_allvars,      wealth_index_score),
            cor_pca_allvars_mr = cor(pca_allvars_mr, wealth_index_score)) %>%
  ungroup() %>%
  pivot_longer(cols = -country_code) %>%
  ggplot(aes(x = value,
             y = name)) +
  geom_boxplot() +
  xlim(0.5, 1)

