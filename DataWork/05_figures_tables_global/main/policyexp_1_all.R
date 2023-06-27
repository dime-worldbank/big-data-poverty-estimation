# Policy Experiment - All Data

# Load data --------------------------------------------------------------------
dhs_df <- readRDS(file.path(dhs_all_exp_dir, "FinalData", "Individual Datasets", "survey_socioeconomic.Rds"))

dhs_ml_pred_df <- readRDS(file.path(data_dir, "DHS", "FinalData", "Merged Datasets",
                                    "survey_alldata_clean_predictions.Rds"))

# Restrict to most recent 3 years ----------------------------------------------
cnt_yr_df <- dhs_df %>%
  
  # 3 most recent years
  distinct(country_code, year) %>%
  group_by(country_code) %>%
  slice_max(order_by = year, n = 3) %>%
  ungroup() %>%
  
  # With 3 years
  group_by(country_code) %>%
  mutate(n_year = n()) %>%
  filter(n_year == 3) %>%
  
  # Add variable to filter full dataset on
  mutate(recent_3_year = 1)

dhs_df <- dhs_df %>%
  left_join(cnt_yr_df, by = c("country_code", "year")) %>%
  filter(recent_3_year %in% 1)

# Merge in ML prediction -------------------------------------------------------
dhs_ml_pred_df <- dhs_ml_pred_df %>%
  rename(pca_predict = predict_pca_allvars_mr_global_country_pred_all) %>%
  dplyr::select(uid, pca_predict)

dhs_df <- dhs_df %>%
  left_join(dhs_ml_pred_df, by = "uid")

# OOS Prediction ---------------------------------------------------------------
## Aggregate and Compute lags
adm_df <- dhs_df %>%
  group_by(country_code, GID_1, year) %>%
  dplyr::summarise(pca_allvars = mean(pca_allvars),
                   pca_predict = mean(pca_predict, na.rm = T)) %>%
  ungroup() %>%
  arrange(year) %>%
  
  group_by(country_code, GID_1) %>%
  mutate(year_lag = lag(year),
         pca_allvars_lag = lag(pca_allvars),
         n_gid = n(),
         yr_id = 1:n()) %>%
  ungroup() %>%
  
  filter(n_gid == 3) %>%
  
  mutate(year_diff = year - year_lag,
         pca_diff = pca_allvars - pca_allvars_lag) 

#### Add in NTL 
ntl_df <- readRDS(file.path(ntl_harmon_dir, "FinalData", "ntl_harmon_gadm1.Rds"))

adm_df <- adm_df %>%
  left_join(ntl_df, by = c("GID_1", "year")) %>%
  mutate(ntl_harmon_log = log(ntl_harmon + 1),
         ntl_harmon_sq = ntl_harmon^2,
         ntl_harmon_log_sq = ntl_harmon_log^2)

#### NTL Prediction
adm_df <- map_df(unique(adm_df$country_code), function(cc){
  
  adm_df_i <- adm_df[adm_df$country_code %in% cc,]
  
  ntl_lm <- lm(pca_allvars ~ ntl_harmon, data = adm_df_i[adm_df_i$yr_id %in% 1:2,])
  
  adm_df_i$pca_ntl_predict <- predict(ntl_lm, adm_df_i) %>% as.numeric()
  
  return(adm_df_i)
})

#### Pivot
adm_df <- adm_df %>%
  pivot_wider(id_cols = c(GID_1, country_code),
              names_from = yr_id,
              values_from = c(year_diff, pca_diff, pca_allvars, year, pca_predict, pca_ntl_predict))

## Extrapolate
adm_df <- adm_df %>%
  mutate(pca_annual_change_2 = pca_diff_2 / year_diff_2,
         pca_allvars_oos_3 = pca_allvars_2 + pca_annual_change_2*year_diff_3)

# Results ----------------------------------------------------------------------
cor_wide_df <- adm_df %>% 
  group_by(country_code) %>%
  dplyr::summarise(r2_oos = cor(pca_allvars_oos_3, pca_allvars_3)^2,
                   r2_ntl = cor(pca_ntl_predict_3, pca_allvars_3)^2,
                   r2_ml = cor(pca_predict_3, pca_allvars_3)^2) %>%
  ungroup() %>%
  mutate(r2_ml_oos_diff = r2_ml - r2_oos,
         r2_ml_ntl_diff = r2_ml - r2_ntl)

cor_long_df <- cor_wide_df %>%
  pivot_longer(cols = -country_code)

cor_long_df %>%
  dplyr::filter(!(name %>% str_detect("diff"))) %>%
  ggplot() +
  geom_boxplot(aes(x = value, y = name))

cor_wide_df %>%
  ggplot() +
  geom_histogram(aes(x = r2_ml_oos_diff),
                 color = "black",
                 fill = "dodgerblue2")

cor_wide_df %>%
  ggplot() +
  geom_histogram(aes(x = r2_ml_ntl_diff))

