# Main Results

FILL_COLOR <- "gray80"

# Load data --------------------------------------------------------------------
# results_df <- readRDS(file.path(data_dir, SURVEY_NAME, "FinalData", "pov_estimation_results",
#                                 "accuracy_appended.Rds"))
results_df <- readRDS(file.path(data_dir, SURVEY_NAME, "FinalData", "pov_estimation_results",
                                "accuracy_appended_bestparam.Rds"))

results_df <- results_df %>%
  dplyr::filter(level_change %in% "levels",
                estimation_type %in% "best",
                target_var != "pca_allvars") %>%
  mutate(target_var_clean = target_var_clean %>% as.character()) %>%
  mutate(target_var_clean = case_when(
    target_var_clean == "Asset Index - All Periods" ~ "Asset Index",
    TRUE ~ target_var_clean
  )) %>%
  mutate(target_var_clean = target_var_clean %>%
           factor(levels = c("DHS Wealth Index",
                             "Asset Index",
                             "Asset Index: Non-Physical Assets",
                             "Asset Index: Physical Assets")))

# results_df <- results_df %>%
#   dplyr::filter(xg_param_set %in% "10_0_1_4_50_0_3_reg_squarederror")

# Arrange ----------------------------------------------------------------------
p <- results_df %>%
  dplyr::mutate(target_var_clean = fct_rev(target_var_clean)) %>%
  ggplot(aes(x = reorder(feature_type_clean, r2, FUN = median, .desc =TRUE),
             y = r2,
             fill = target_var_clean)) +
  geom_boxplot(errorbar.draw = FALSE, center = TRUE) +
  #geom_half_point(transformation = position_jitter(width = 0.05, height = 0.1)) +
  #stat_summary(fun = median, geom = "text", col = "black",     # Add text to plot
  #             vjust = -0.2, aes(label = paste(round(..y.., digits = 2)))) +
  #stat_summary(fun = max, geom = "text", col = "firebrick3",    
  #             vjust = -0.2, aes(label = paste(round(..y.., digits = 2)))) +
  labs(x = NULL,
       y = expression(r^2),
       title = "Performance across target variables using different features",
       fill = "Target variable") +
  scale_y_continuous(limits = c(0,1)) +
  scale_fill_manual(values = c("chartreuse3",
                               "darkorange",
                               "deepskyblue",
                               "dodgerblue2"),
                    guide = guide_legend(reverse = TRUE) ) +
  theme_classic() +
  theme(axis.text.y = element_text(face = "bold"),
        plot.title = element_text(face = "bold"),
        plot.title.position = "plot") +  
  coord_flip() 

# Export -----------------------------------------------------------------------
ggsave(p, 
       filename = file.path(figures_global_dir, "accuracy_featuretype_targetvar.png"),
       height = 8,
       width = 8)




