# Policy Experiment 2: Nigeria

# Load data --------------------------------------------------------------------
dhs_df <- readRDS(file.path(data_dir, "DHS_nga_policy_experiment", "FinalData", "Merged Datasets", "survey_alldata.Rds"))
dhs_df$GID_1 <- dhs_df$GID_2

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
  group_by(year, GID_1) %>%
  dplyr::summarise(pca_allvars_predict = mean(pca_allvars_predict),
                   pca_allvars = mean(pca_allvars)) %>%
  ungroup() %>%
  
  arrange(year) %>%
  group_by(GID_1) %>%
  mutate(year_id = 1:n(),
         year_lag = lag(year),
         pca_allvars_lag = lag(pca_allvars),) %>%
  ungroup() %>%
  
  mutate(year_diff = year - year_lag,
         pca_diff = pca_allvars - pca_allvars_lag) 

gadm_wide_df <- gadm_long_df %>%
  pivot_wider(id_cols = GID_1,
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
  dplyr::select(GID_1, 
                pca_allvars_oos_1, 
                pca_allvars_oos_2,
                pca_allvars_oos_3,
                pca_allvars_oos_4) %>%
  pivot_longer(cols = -GID_1) %>%
  mutate(year = case_when(
    name == "pca_allvars_oos_1" ~ 2003,
    name == "pca_allvars_oos_2" ~ 2008,
    name == "pca_allvars_oos_3" ~ 2013,
    name == "pca_allvars_oos_4" ~ 2018
  )) %>%
  dplyr::rename(pca_allvars_oos = value) %>%
  dplyr::select(GID_1, pca_allvars_oos, year)

# NTL Estimates ----------------------------------------------------------------

# Analysis Datasets ------------------------------------------------------------
gadm_yr_df <- dhs_df %>%
  group_by(GID_1, year) %>%
  dplyr::summarise(pca_allvars_predict = mean(pca_allvars_predict),
                   pca_allvars = mean(pca_allvars)) %>%
  ungroup() %>%
  left_join(gadm_oos_long_df, by = c("GID_1", "year")) 

gadm_yr_long_df <- gadm_yr_df %>%
  pivot_longer(cols = -c(GID_1, year, pca_allvars))

yr_long_df <- dhs_df %>%
  group_by(year) %>%
  dplyr::summarise(pca_allvars_predict_avg = mean(pca_allvars_predict),
                   pca_allvars_avg = mean(pca_allvars),
                   
                   pca_allvars_predict_p95 = quantile(pca_allvars_predict, 0.8),
                   pca_allvars_p95 = quantile(pca_allvars, 0.8),
                   
                   pca_allvars_predict_p05 = quantile(pca_allvars_predict, 0.2),
                   pca_allvars_p05 = quantile(pca_allvars, 0.2)) %>%
  ungroup() %>%
  pivot_longer(cols = -year) %>%
  mutate(name_clean = case_when(
    name == "pca_allvars_predict_avg" ~ "Estimated",
    name == "pca_allvars_avg" ~ "True",
    name == "pca_allvars_predict_p95" ~ "Estimated",
    name == "pca_allvars_p95" ~ "True",
    name == "pca_allvars_predict_p05" ~ "Estimated",
    name == "pca_allvars_p05" ~ "True"
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
  ggplot(aes(x = year, y = average,
             color = name_clean)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  scale_color_manual(values = c("dodgerblue2", "darkorange")) +
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

