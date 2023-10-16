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

