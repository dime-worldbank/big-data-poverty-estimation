# Main Results

FILL_COLOR <- "gray80"

# Load data --------------------------------------------------------------------
results_df <- readRDS(file.path(data_dir, SURVEY_NAME, "FinalData", "pov_estimation_results",
                                "accuracy_appended_bestparam_within_country_cv_levels.Rds"))

results_df <- results_df %>%
  dplyr::filter(level_change %in% "levels",
                !(target_var %in% "pca_allvars"))

# 1. All features, best estimation type ----------------------------------------
p_trainsample <- results_df %>%
  dplyr::filter(feature_type %in% "all",
                target_var %in% "pca_allvars_mr") %>%
  ggplot(aes(x = reorder(estimation_type_clean, r2, FUN = median, .desc =TRUE),
             y = r2)) +
  geom_half_boxplot(errorbar.draw = FALSE, center = TRUE, 
                    fill = FILL_COLOR) +
  #geom_half_point(transformation = position_jitter(width = 0.05, height = 0.1)) +
  stat_summary(fun = median, geom = "text", col = "black",     
               vjust = -0.2, aes(label = paste(round(..y.., digits = 2)))) +
  stat_summary(fun = max, geom = "text", col = "firebrick3",    
               vjust = -0.2, aes(label = paste(round(..y.., digits = 2)))) +
  labs(x = NULL,
       y = expression(r^2),
       title = "A. Performance by training sample type") +
  scale_y_continuous(limits = c(0,1)) +
  theme_classic() +
  theme(legend.position = "none",
        axis.text.y = element_text(face = "bold"),
        plot.title = element_text(face = "bold"),
        plot.title.position = "plot") +
  coord_flip() 

# 2. By target variable --------------------------------------------------------
p_targetvar <- results_df %>%
  dplyr::filter(feature_type %in% "all",
                estimation_type %in% "within_country_cv") %>%
  ggplot(aes(x = reorder(target_var_clean, r2, FUN = median, .desc =TRUE),
             y = r2)) +
  geom_half_boxplot(errorbar.draw = FALSE, center = TRUE,
                    fill = FILL_COLOR) +
  #geom_half_point(transformation = position_jitter(width = 0.05, height = 0.1)) +
  stat_summary(fun = median, geom = "text", col = "black",     # Add text to plot
               vjust = -0.2, aes(label = paste(round(..y.., digits = 2)))) +
  stat_summary(fun = max, geom = "text", col = "firebrick3",    
               vjust = -0.2, aes(label = paste(round(..y.., digits = 2)))) +
  labs(x = NULL,
       y = expression(r^2),
       title = "B. Performance by outcome variable") +
  scale_y_continuous(limits = c(0,1)) +
  theme_classic() +
  theme(legend.position = "none",
        axis.text.y = element_text(face = "bold"),
        plot.title = element_text(face = "bold"),
        plot.title.position = "plot") +
  coord_flip() 

# 3. By feature ----------------------------------------------------------------
p_feature <- results_df %>%
  dplyr::filter(target_var %in% "pca_allvars_mr",
                estimation_type %in% "best") %>%
  ggplot(aes(x = reorder(feature_type_clean, r2, FUN = median, .desc =TRUE),
             y = r2)) +
  geom_half_boxplot(errorbar.draw = FALSE, center = TRUE,
                    fill = FILL_COLOR) +
  #geom_half_point(transformation = position_jitter(width = 0.05, height = 0.1)) +
  stat_summary(fun = median, geom = "text", col = "black",     # Add text to plot
               vjust = -0.2, aes(label = paste(round(..y.., digits = 2)))) +
  stat_summary(fun = max, geom = "text", col = "firebrick3",    
               vjust = -0.2, aes(label = paste(round(..y.., digits = 2)))) +
  labs(x = NULL,
       y = expression(r^2),
       title = "C. Performance using different features") +
  scale_y_continuous(limits = c(0,1)) +
  theme_classic() +
  theme(legend.position = "none",
        axis.text.y = element_text(face = "bold"),
        plot.title = element_text(face = "bold"),
        plot.title.position = "plot") +  
  coord_flip() 

# Arrange ----------------------------------------------------------------------
p_l <- ggarrange(p_trainsample,
                 p_targetvar,
                 nrow = 2)

p <- ggarrange(p_l, 
               p_feature,
               nrow = 1)

ggsave(p, 
       filename = file.path(figures_global_dir, "performence_country_avg_types.png"),
       height = 6,
       width = 10)




