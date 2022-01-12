# TODO:
# Could check if this correlation works better in countries with more Facebook penetration

df <- readRDS(file.path(data_dir, "DHS", "FinalData", "Merged Datasets", "survey_alldata_clean.Rds"))

fb_param_df <- readRDS(file.path(data_dir, "Facebook Marketing", "FinalData", "facebook_marketing_parameters_clean.Rds"))

df <- df %>%
  dplyr::filter(fb_estimate_mau_upper_bound_3 > 30000)

df %>%
ggplot() +
  geom_point(aes(x = educ_years_hh_max,
                 y = fb_prop_estimate_mau_upper_bound_3))

cor.test(df$pca_allvars,       df$fb_prop_estimate_mau_upper_bound_3)
cor.test(df$educ_years_hh_max, df$fb_prop_estimate_mau_upper_bound_3)


df$educ_years_hh_max