# Policy Experiment 2: Nigeria

# Load data --------------------------------------------------------------------
dhs_df <- readRDS(file.path(data_dir, "DHS_nga_policy_experiment", "FinalData", "Merged Datasets", "survey_alldata.Rds"))

# Predict Wealth ---------------------------------------------------------------
#### Check Results
# models_all <- file.path(data_dir, "DHS", "FinalData", "pov_estimation_results", "models") %>%
#   list.files() %>% str_subset("levels_changevars_ng")
# 
# result_df <- map_df(models_all, function(model_i){
#   print(model_i)
#   ml_model <- readRDS(file.path(data_dir, "DHS", "FinalData", "pov_estimation_results", "models",
#                                 model_i))
#   
#   X <- dhs_df %>%
#     dplyr::select_at(vars(starts_with("ntlharmon_"),
#                           starts_with("cnn_ntlharmon_landsat_"),
#                           starts_with("l7_"),
#                           starts_with("gc_"),
#                           starts_with("weather_"),
#                           starts_with("pollution_aod_"))) %>%
#     as.matrix()
#   
#   if(model_i %>% str_detect("_svm_")){
#     dhs_df$pca_allvars_predict <- predict(ml_model, X)[[1]]
#   } else if(model_i %>% str_detect("_glmnet_")){
#     dhs_df$pca_allvars_predict <- predict(ml_model$cv_model, 
#                                           s = ml_model$optimal_lambda, 
#                                           newx = X) %>% as.numeric()
#   } else{
#     dhs_df$pca_allvars_predict <- predict(ml_model, X)
#   }
# 
#   pca_cor <- cor(dhs_df$pca_allvars_predict, dhs_df$pca_allvars)
#   
#   dhs_sum_df <- dhs_df %>%
#     group_by(year) %>%
#     dplyr::summarise(pca_allvars_predict = mean(pca_allvars_predict),
#                      pca_allvars = mean(pca_allvars)) %>%
#     ungroup()
#   
#   gadm_cor <- cor(dhs_sum_df$pca_allvars_predict, dhs_sum_df$pca_allvars)
#   
#   data.frame(pca_cor = pca_cor,
#              gadm_cor = gadm_cor,
#              model = model_i)
# })
# 
# #### Predict with Best Model
# model_best <- result_df %>%
#   dplyr::filter(!is.na(pca_cor)) %>%
#   arrange(-pca_cor) %>%
#   head(1) %>%
#   pull(model)
# 
# ml_model <- readRDS(file.path(data_dir, "DHS", "FinalData", "pov_estimation_results", "models",
#                               model_best))

model_use <- file.path(data_dir, "DHS", "FinalData", "pov_estimation_results", "models") %>%
  list.files() %>% 
  str_subset("levels_changevars_ng") %>%
  str_subset("xgboost") %>%
  str_subset("global_country")

ml_model <- readRDS(file.path(data_dir, "DHS", "FinalData", "pov_estimation_results", "models",
                              model_use))

X <- dhs_df %>%
  dplyr::select_at(vars(starts_with("ntlharmon_"),
                        starts_with("cnn_ntlharmon_landsat_"),
                        starts_with("l7_"),
                        starts_with("gc_"),
                        starts_with("weather_"),
                        starts_with("pollution_aod_"))) %>%
  as.matrix()

if(model_use %>% str_detect("_svm_")){
  dhs_df$pca_allvars_predict <- predict(ml_model, X)[[1]]
} else if(model_use %>% str_detect("_glmnet_")){
  dhs_df$pca_allvars_predict <- predict(ml_model$cv_model, 
                                        s = ml_model$optimal_lambda, 
                                        newx = X) %>% as.numeric()
} else{
  dhs_df$pca_allvars_predict <- predict(ml_model, X)
}

pca_cor <- cor(dhs_df$pca_allvars_predict, dhs_df$pca_allvars)

# OOS Estimates ----------------------------------------------------------------
gadm_long_df <- dhs_df %>%
  group_by(year, GID_2) %>%
  dplyr::summarise(pca_allvars_predict = mean(pca_allvars_predict),
                   pca_allvars = mean(pca_allvars)) %>%
  ungroup() %>%
  
  arrange(year) %>%
  group_by(GID_2) %>%
  mutate(year_id = 1:n(),
         year_lag = lag(year),
         pca_allvars_lag = lag(pca_allvars),) %>%
  ungroup() %>%
  
  mutate(year_diff = year - year_lag,
         pca_diff = pca_allvars - pca_allvars_lag) 

gadm_wide_df <- gadm_long_df %>%
  pivot_wider(id_cols = GID_2,
              names_from = year_id,
              values_from = c(year,
                              pca_allvars,
                              pca_allvars_predict,
                              year_diff,
                              pca_diff))

gadm_wide_df <- gadm_wide_df %>%
  mutate(pca_allvars_oos_1 = pca_allvars_2 - (pca_diff_3 / year_diff_3)*year_diff_2,
         pca_allvars_oos_2 = (pca_allvars_1 + pca_allvars_3)/2,
         pca_allvars_oos_3 = (pca_allvars_2 + pca_allvars_4)/2,
         pca_allvars_oos_4 = pca_allvars_3 + (pca_diff_3 / year_diff_3)*year_diff_4)

gadm_oos_long_df <- gadm_wide_df %>%
  dplyr::select(GID_2, 
                pca_allvars_oos_1, 
                pca_allvars_oos_2,
                pca_allvars_oos_3,
                pca_allvars_oos_4) %>%
  pivot_longer(cols = -GID_2) %>%
  mutate(year = case_when(
    name == "pca_allvars_oos_1" ~ 2003,
    name == "pca_allvars_oos_2" ~ 2008,
    name == "pca_allvars_oos_3" ~ 2013,
    name == "pca_allvars_oos_4" ~ 2018
  )) %>%
  dplyr::rename(pca_allvars_oos = value) %>%
  dplyr::select(GID_2, pca_allvars_oos, year)

# NTL Estimates ----------------------------------------------------------------
# #### Prep data
# dhs_all_df <- readRDS(file.path(dhs_dir, "FinalData", "Individual Datasets", 
#                             "survey_socioeconomic.Rds"))
# 
# ntl_all_df <- readRDS(file.path(ntl_harmon_dir, "FinalData", "ntl_harmon_gadm2.Rds"))
# 
# ntl_gadm_all_df <- dhs_all_df %>%
#   dplyr::filter(most_recent_survey %in% T) %>%
#   group_by(country_code, GID_2, year) %>%
#   dplyr::summarise(pca_allvars = mean(pca_allvars)) %>%
#   ungroup() %>%
#   left_join(ntl_df, by = c("GID_2", "year"))
# 
# gadm_long_ntl_df <- gadm_long_df %>%
#   left_join(ntl_all_df,
#             by = c("GID_2", "year"))
# 
# ntl_lm <- lm(pca_allvars ~ ntl_harmon, data = ntl_gadm_all_df[ntl_gadm_all_df$country_code != "NG",])
# 
# gadm_long_ntl_df$pca_ntl_predict <- predict(ntl_lm, gadm_long_ntl_df) %>% as.numeric()
# 
# #### NTL Prediction
# gadm_long_ntl_df <- gadm_long_ntl_df %>%
#   dplyr::select(GID_2, year, pca_ntl_predict)

#### Prep data
ntl_all_df <- readRDS(file.path(ntl_harmon_dir, "FinalData", "ntl_harmon_gadm2.Rds"))

gadm_long_df <- gadm_long_df %>%
  left_join(ntl_all_df, by = c("GID_2", "year"))

gadm_long_ntl_df <- map_df(unique(gadm_long_df$year), function(year_i){
  
  ntl_lm <- lm(pca_allvars ~ ntl_harmon, data = gadm_long_df[gadm_long_df$year != year_i,])
  
  gadm_long_df_yri <- gadm_long_df[gadm_long_df$year %in% year_i,]
  gadm_long_df_yri$pca_ntl_predict <- predict(ntl_lm, gadm_long_df_yri) %>% as.numeric()
  
  return(gadm_long_df_yri)
})

#### NTL Prediction
gadm_long_ntl_df <- gadm_long_ntl_df %>%
  dplyr::select(GID_2, year, pca_ntl_predict)

# Analysis Datasets ------------------------------------------------------------
gadm_yr_df <- dhs_df %>%
  group_by(GID_2, year) %>%
  dplyr::summarise(pca_allvars_predict = mean(pca_allvars_predict),
                   pca_allvars = mean(pca_allvars)) %>%
  ungroup() %>%
  left_join(gadm_oos_long_df, by = c("GID_2", "year")) %>%
  left_join(gadm_long_ntl_df, by = c("GID_2", "year"))  

gadm_yr_long_df <- gadm_yr_df %>%
  pivot_longer(cols = -c(GID_2, year, pca_allvars))

yr_est_df <- gadm_yr_df %>%
  group_by(year) %>%
  dplyr::summarise(pca_allvars_oos = mean(pca_allvars_oos, na.rm=T),
                   pca_ntl_predict = mean(pca_ntl_predict, na.rm=T)) %>%
  ungroup()

yr_long_df <- dhs_df %>%
  group_by(year) %>%
  dplyr::summarise(pca_allvars_predict_avg = mean(pca_allvars_predict),
                   pca_allvars_avg = mean(pca_allvars),
                   
                   pca_allvars_predict_p95 = quantile(pca_allvars_predict, 0.8),
                   pca_allvars_p95 = quantile(pca_allvars, 0.8),
                   
                   pca_allvars_predict_p05 = quantile(pca_allvars_predict, 0.2),
                   pca_allvars_p05 = quantile(pca_allvars, 0.2)) %>%
  ungroup() %>%
  
  left_join(yr_est_df, by = "year") %>%
  
  pivot_longer(cols = -year) %>%
  mutate(name_clean = case_when(
    name == "pca_allvars_predict_avg" ~ "ML Model Estimate",
    name == "pca_allvars_avg" ~ "True",
    name == "pca_allvars_predict_p95" ~ "ML Model Estimate",
    name == "pca_allvars_p95" ~ "True",
    name == "pca_allvars_predict_p05" ~ "ML Model Estimate",
    name == "pca_allvars_p05" ~ "True",
    name == "pca_allvars_oos" ~ "DHS Interpolation/Extrapolation",
    name == "pca_ntl_predict" ~ "NTL Reg Estimate"
  )) %>%
  mutate(type = case_when(
    name %>% str_detect("_p95") ~ "p95",
    name %>% str_detect("_p05") ~ "p05",
    TRUE ~ "average"
  )) %>%
  mutate(name = name %>% str_replace_all("_p95|_p05|_avg", "")) %>%
  pivot_wider(id_cols = c(year, name, name_clean),
              names_from = type,
              values_from = value)

# Figures ----------------------------------------------------------------------
p_trends <- yr_long_df %>%
  # dplyr::filter(name_clean %in% c("Estimated",
  #                                 "True")) %>%
  ggplot(aes(x = year, y = average,
             color = name_clean)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  scale_color_manual(values = c("gray", "dodgerblue2", "darkorange", "black")) +
  scale_x_continuous(breaks = c(2003, 2008, 2013, 2018)) +
  theme_classic2() +
  theme(plot.title = element_text(face = "bold", size = 10)) +
  labs(x = NULL,
       y = "Wealth",
       color = "Wealth",
       title = "A. Trends in True and Estimated Wealth")

p_scatter <- gadm_yr_long_df %>%
  mutate(name = case_when(
    name == "pca_allvars_predict" ~ "Estimate from\nML Model",
    name == "pca_allvars_oos" ~ "Survey\nInterpolation/Extrapolation", 
    name == "pca_ntl_predict" ~ "Estimate from\nNTL Reg."
  )) %>%
  
  ggplot(aes(x = value, y = pca_allvars)) +
  geom_point(size = 0.25) +
  geom_smooth(method = lm, se = F, color = "darkorange") +
  # stat_cor(aes(label = paste(gsub("R", "r", ..rr.label..), ..p.label.., sep = "*`,`~")),
  #          label.x.npc = "left",
  #          color = "red",
  #          size = 3) +
  stat_cor(aes(label = paste(gsub("R", "r", ..rr.label..))),
           label.x.npc = "left",
           color = "red",
           size = 3) +
  facet_grid(year~name) +
  theme_classic() +
  theme(strip.background = element_blank(),
        strip.text = element_text(face = "bold"),
        plot.title = element_text(face = "bold", size = 10)) +
  scale_x_continuous(limits = c(-5, 5)) +
  scale_y_continuous(limits = c(-5, 5)) +
  labs(x = "Estimated Wealth",
       y = "True Wealth",
       title = "B. Comparison of ML model estimate and\nsurvey interpolation/extrapolation")

# Arrange/Export ---------------------------------------------------------------
p_all <- ggarrange(p_trends, 
                   p_scatter,
                   nrow = 2,
                   heights = c(0.3, 0.7))

ggsave(p_all,
       filename = file.path(figures_global_dir, "policy_exp_nga.png"),
       height = 6.25,
       width = 4.2)

