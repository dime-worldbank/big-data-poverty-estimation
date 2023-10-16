# Compare ML Model Types

FILL_COLOR <- "gray80" 
TEXT_COLOR <- "firebrick3"

# Load data --------------------------------------------------------------------
xgboost_df <- readRDS(file.path(data_dir, "DHS", "FinalData", "pov_estimation_results",
                                "accuracy_appended.Rds"))

glmnet_df <- readRDS(file.path(data_dir, "DHS", "FinalData", "pov_estimation_results",
                               "accuracy_appended_glmnet.Rds"))

svm_df <- readRDS(file.path(data_dir, "DHS", "FinalData", "pov_estimation_results",
                            "accuracy_appended_svm.Rds"))

model_df <- bind_rows(xgboost_df,
                      glmnet_df,
                      svm_df)

model_df %>%
  dplyr::filter(estimation_type == "global_country_pred",
                feature_type %in% c("all", "all_changes")) %>%
  dplyr::mutate(
    model_type = case_when(
      model_type == "xgboost" ~ "XGBoost",
      model_type == "svm" ~ "SVM",
      model_type == "glmnet" ~ "Regularized\nRegression"
    ),
    level_change = level_change %>% 
      tools::toTitleCase() %>% 
      factor() %>%
      fct_rev()) %>%
  ggplot(aes(y = r2,
             x = model_type)) +
  geom_half_boxplot(errorbar.draw = FALSE, center = TRUE,
                    fill = FILL_COLOR) +
  stat_summary(fun = median, geom = "text", col = "black",     # Add text to plot
               vjust = -0.2, size = 3, hjust = 0.5, aes(label = paste(round(..y.., digits = 2)))) +
  stat_summary(fun = p75, geom = "text", col = TEXT_COLOR,     # Add text to plot
               vjust = -0.2, size = 3, hjust = -0.05, aes(label = paste(round(..y.., digits = 2)))) +
  stat_summary(fun = p25, geom = "text", col = TEXT_COLOR,     # Add text to plot
               vjust = -0.2, size = 3, hjust = -0.05, aes(label = paste(round(..y.., digits = 2)))) +
  facet_wrap(~level_change,
             scales = "free_x",
             ncol = 1) +
  labs(x = "Model\nType",
       y = expression(r^2)) +
  theme_classic2() +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.5),
        axis.text.y = element_text(face = "bold", color = "black"),
        strip.background = element_blank(),
        strip.text = element_text(face = "bold")) +
  coord_flip() 

ggsave(filename = file.path(figures_global_dir, "ml_results_type.png"),
       height = 4,
       width = 5.5)

# 
# 
# 
# 
# 
# # Load data --------------------------------------------------------------------
# acc_df <- readRDS(file.path(data_dir, "DHS", "FinalData", "pov_estimation_results",
#                             "accuracy_appended.Rds"))
# 
# # # District ---------------------------------------------------------------------
# pred_df <- file.path(data_dir, SURVEY_NAME, "FinalData", "pov_estimation_results", "predictions") %>%
#   list.files(pattern = "*.Rds",
#              full.names = T) %>%
#   #str_subset("changes") %>%
#   map_df(read_add_file) %>%
#   dplyr::filter(level_change != "levels_changevars_ng") %>%
#   dplyr::filter(#estimation_type %in% "global_country_pred",
#     feature_type %in% c("all", "all_changes"))
# 
# #### Survey
# survey_df <- readRDS(file.path(data_dir, SURVEY_NAME, "FinalData", "Merged Datasets", "survey_alldata_clean_changes_cluster.Rds"))
# survey_df <- survey_df %>%
#   dplyr::select(uid, gadm_uid, continent_adj, country_name, iso2)
# 
# pred_df <- pred_df %>%
#   left_join(survey_df, by = "uid") %>%
#   group_by(country_code, gadm_uid, ml_model_type, glmnet_alpha, level_change, feature_type) %>%
#   dplyr::summarise(truth = mean(truth),
#                    prediction = mean(prediction)) %>%
#   ungroup() %>%
#   
#   group_by(country_code, ml_model_type, glmnet_alpha, level_change, feature_type) %>%
#   dplyr::summarise(r2 = cor(truth, prediction)^2)
# 
# # Cleanup ----------------------------------------------------------------------
# # within_country_cv
# acc_df <- acc_df %>%
#   dplyr::filter(estimation_type %in% "global_country_pred",
#                 feature_type %in% c("all", "all_changes")) %>%
#   dplyr::mutate(ml_model_type = case_when(
#     ml_model_type == "svm" ~ "SVM",
#     ml_model_type == "xgboost" ~ "XGBoost",
#     (ml_model_type == "glmnet" & glmnet_alpha == 1) ~ "Lasso",
#     (ml_model_type == "glmnet" & glmnet_alpha == 0) ~ "Ridge"
#   )) %>%
#   dplyr::mutate(level_change = case_when(
#     level_change == "levels" ~ "Levels",
#     level_change == "changes" ~ "Changes"
#   ) %>% fct_rev()) #%>%
# 
# # group_by(country_code, ml_model_type, level_change) %>%
# # slice_max(r2, n = 1, with_ties = F) %>%
# # ungroup()
# 
# # Figure -----------------------------------------------------------------------
# p <- acc_df %>%
#   ggplot(aes(x = ml_model_type,
#              y = r2)) +
#   geom_half_boxplot(errorbar.draw = FALSE, center = TRUE,
#                     fill = FILL_COLOR) +
#   stat_summary(fun = median, geom = "text", col = "black",     # Add text to plot
#                vjust = -0.2, size = 3.5, hjust = 0.5, aes(label = paste(round(..y.., digits = 2)))) +
#   stat_summary(fun = p75, geom = "text", col = "black",     # Add text to plot
#                vjust = -0.2, size = 3.5, hjust = -0.05, aes(label = paste(round(..y.., digits = 2)))) +
#   # stat_summary(fun = max, geom = "text", col = "firebrick3",    
#   #              vjust = -0.2, aes(label = paste(round(..y.., digits = 2)))) +
#   # stat_summary(fun = min, hjust = 0.05, geom = "text", col = "firebrick3",   
#   #              vjust = -0.2, aes(label = paste(round(..y.., digits = 2)))) +
#   facet_wrap(~level_change,
#              scales = "free_x") +
#   theme_classic() +
#   theme(legend.position = "none",
#         axis.text.y = element_text(face = "bold"),
#         plot.title = element_text(face = "bold"),
#         plot.title.position = "plot",
#         strip.background = element_blank(),
#         strip.text = element_text(face = "bold", size = 12)) +  
#   coord_flip() +
#   labs(x = NULL,
#        y = expression(r^2),
#        title = "Performance by Machine Learning Algorithm") 
# 
# ggsave(p,
#        filename = file.path(figures_global_dir, "ml_results_type.png"),
#        height = 4.5,
#        width = 7)
# 
# 
