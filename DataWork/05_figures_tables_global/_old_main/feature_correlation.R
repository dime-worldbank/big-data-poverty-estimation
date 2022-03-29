# Feature Correlation

# Load Data --------------------------------------------------------------------
df <- readRDS(file.path(data_dir, SURVEY_NAME, "FinalData", "Merged Datasets", "survey_alldata_clean.Rds"))

# Correlation Dataframe --------------------------------------------------------
df_long <- df %>%
  dplyr::select(country_name,
                pca_allvars, 
                contains("fb_prop_")) %>%
  pivot_longer(cols = -c(country_name, pca_allvars))

df_cor <- df_long %>%
  dplyr::filter(!is.na(pca_allvars),
                !is.na(value)) %>%
  group_by(name, country_name) %>%
  dplyr::summarise(cor = cor(pca_allvars, value))

# Cleanup Variable Names -------------------------------------------------------
fb_param_df <- readRDS(file.path(data_dir, "Facebook Marketing", "FinalData", "facebook_marketing_parameters_clean.Rds"))

fb_param_df <- fb_param_df %>%
  dplyr::mutate(fb_param_id = paste0("fb_id_", param_id)) %>%
  dplyr::select(fb_param_id, param_name_clean, param_category, param_name_simple)

df_cor <- df_cor %>%
  dplyr::mutate(fb_param_id = name %>% str_replace_all("fb_prop_estimate_mau_", "fb_id_")) %>%
  left_join(fb_param_df, by = "fb_param_id")

# Figure -----------------------------------------------------------------------
df_cor %>%
  dplyr::filter(!is.na(cor)) %>%
  ggplot() +
  geom_boxplot(aes(y = reorder(param_name_clean, cor, median),
                   x = cor)) +
  labs(x = NULL,
       y = "Correlation")






