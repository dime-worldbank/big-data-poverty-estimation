# ML Changes
# Boxplots by type


FILL_COLOR <- "gray80" 

# For adding text onto boxplot
p75 <- function(x) quantile(x, probs = 0.75) %>% as.numeric()

# Load data --------------------------------------------------------------------
df <- readRDS(file.path(data_dir, SURVEY_NAME, "FinalData", "pov_estimation_results",
                        "accuracy_appended_bestparam.Rds"))

df <- df %>%
  dplyr::filter(level_change %in% "changes")

# Boxplots: By Training Sample -------------------------------------------------
p_boxplot_tsample <- df %>%
  dplyr::filter(feature_type %in% "all_changes",
                target_var %in% "pca_allvars") %>%
  ggplot(aes(x = reorder(estimation_type_clean, r2, FUN = mean, .desc =TRUE),
             y = r2)) +
  geom_half_boxplot(errorbar.draw = FALSE, center = TRUE,
                    fill = FILL_COLOR) +
  #geom_half_point(transformation = position_jitter(width = 0.05, height = 0.1)) +
  stat_summary(fun = median, geom = "text", col = "black",     # Add text to plot
               vjust = -0.2, hjust = 0.5, aes(label = paste(round(..y.., digits = 2)))) +
  stat_summary(fun = p75, geom = "text", col = "black",     # Add text to plot
               vjust = -0.2, hjust = -0.05, aes(label = paste(round(..y.., digits = 2)))) +
  stat_summary(fun = max, geom = "text", col = "firebrick3",    
               vjust = -0.2, aes(label = paste(round(..y.., digits = 2)))) +
  labs(x = NULL,
       y = expression(r^2),
       title = "A. Performance by training sample type") +
  #scale_y_continuous(limits = c(0,0.6)) +
  theme_classic() +
  theme(legend.position = "none",
        axis.text.y = element_text(face = "bold"),
        plot.title = element_text(face = "bold"),
        plot.title.position = "plot") +  
  coord_flip() 

# Boxplots: By Target Variable -------------------------------------------------
p_boxplot_tvar <- df %>%
  dplyr::filter(feature_type %in% "all_changes",
                estimation_type %in% "best") %>%
  ggplot(aes(x = reorder(target_var_clean, r2, FUN = mean, .desc =TRUE),
             y = r2)) +
  geom_half_boxplot(errorbar.draw = FALSE, center = TRUE,
                    fill = FILL_COLOR) +
  geom_half_point(transformation = position_jitter(width = 0.05, height = 0.005)) +
  stat_summary(fun = median, geom = "text", col = "black",     # Add text to plot
               vjust = -0.2, hjust = 0.5, aes(label = paste(round(..y.., digits = 2)))) +
  stat_summary(fun = p75, geom = "text", col = "black",     # Add text to plot
               vjust = -0.2, hjust = -0.05, aes(label = paste(round(..y.., digits = 2)))) +
  stat_summary(fun = max, geom = "text", col = "firebrick3",    
               vjust = -0.2, aes(label = paste(round(..y.., digits = 2)))) +
  labs(x = NULL,
       y = expression(r^2),
       title = "B. Performance by outcome variable") +
  theme_classic() +
  theme(legend.position = "none",
        axis.text.y = element_text(face = "bold"),
        plot.title = element_text(face = "bold"),
        plot.title.position = "plot") +  
  coord_flip() 

p_hist_tvar <- df %>%
  dplyr::mutate(r2_round = ceiling(r2*10)/10) %>%
  dplyr::mutate(r2_round = case_when(
    r2_round == 0.1 ~ "0 - 0.1",
    r2_round == 0.2 ~ "0.1 - 0.2",
    r2_round == 0.3 ~ "0.2 - 0.3"
  )) %>%
  dplyr::filter(feature_type %in% "all_changes",
                estimation_type %in% "within_country_cv") %>%
  group_by(target_var_clean, r2_round) %>%
  dplyr::summarise(n = n()) %>%
  ungroup() %>%
  group_by(target_var_clean) %>%
  dplyr::mutate(n_total = sum(n)) %>%
  ungroup() %>%
  dplyr::mutate(perc = round(n/n_total*100,1)) %>%
  dplyr::mutate(text = paste0(n, " (", perc, "%)")) %>%
  ggplot(aes(x = r2_round,
             y = n,
             fill = target_var_clean)) +
  geom_col(position = position_dodge(width = 0.94),
           color = "black") +
  geom_text(aes(label = text,
                y = n+3.5),
            position = position_dodge(width = 0.94)) +
  coord_flip() +
  labs(y = "N Countries",
       x = expression(r^2),
       fill = "Variable",
       title = "B. Performance by outcome variable") +
  scale_fill_manual(values = c("lightgoldenrod1",
                               "lightsteelblue"),
                    guide = guide_legend(reverse = TRUE)) +
  theme_classic() +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.5),
        plot.title = element_text(face = "bold")) +
  ylim(0, 40)

# Boxplots: By Feature Set -----------------------------------------------------
p_boxplot_feature <- df %>%
  dplyr::filter(estimation_type %in% "best",
                target_var %in% "pca_allvars") %>%
  dplyr::filter(feature_type_clean != "cnn_viirs_landsat") %>%
  ggplot(aes(x = reorder(feature_type_clean, r2, FUN = mean, .desc =TRUE),
             y = r2)) +
  geom_half_boxplot(errorbar.draw = FALSE, center = TRUE,
                    fill = FILL_COLOR) +
  #geom_half_point(transformation = position_jitter(width = 0.05, height = 0.1)) +
  stat_summary(fun = median, geom = "text", col = "black",     # Add text to plot
               vjust = -0.2, hjust = 0.5, aes(label = paste(round(..y.., digits = 2)))) +
  stat_summary(fun = p75, geom = "text", col = "black",     # Add text to plot
               vjust = -0.2, hjust = -0.01, aes(label = paste(round(..y.., digits = 2)))) +
  stat_summary(fun = max, geom = "text", col = "firebrick3",    
               vjust = -0.2, aes(label = paste(round(..y.., digits = 2)))) +
  labs(x = NULL,
       y = expression(r^2),
       title = "C. Performance using different features") +
  #scale_y_continuous(limits = c(0,0.6)) +
  theme_classic() +
  theme(legend.position = "none",
        axis.text.y = element_text(face = "bold"),
        plot.title = element_text(face = "bold"),
        plot.title.position = "plot") +  
  coord_flip() 

# Arrange/export ---------------------------------------------------------------
p_left <- ggarrange(p_boxplot_tsample,
                    p_hist_tvar,
                    ncol = 1)

p <- ggarrange(p_left, p_boxplot_feature,
               nrow = 1)

ggsave(p,
       filename = file.path(figures_global_dir, "ml_changes_bytype.png"),
       height = 6,
       width = 14)








