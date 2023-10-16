# Changes: Main Figure

FILL_COLOR <- "gray80" 

# For adding text onto boxplot
p75 <- function(x) quantile(x, probs = 0.75) %>% as.numeric()

# Load/prep survey data --------------------------------------------------------
cluster_df <- readRDS(file.path(data_dir, SURVEY_NAME, "FinalData", "Merged Datasets",
                                "survey_alldata_clean_changes_cluster_predictions.Rds"))

district_df <- readRDS(file.path(data_dir, SURVEY_NAME, "FinalData", "Merged Datasets",
                                 "predictions_changes_district_appended.Rds"))
district_df <- district_df %>%
  dplyr::filter(estimation_type %in% "best")

## Add correlation
cluster_df <- cluster_df %>%
  ungroup() %>%
  group_by(country_name) %>%
  dplyr::mutate(r2 = cor(pca_allvars, predict_pca_allvars_best_all_changes)^2) %>%
  ungroup() %>%
  dplyr::mutate(country_name = fct_reorder(country_name, -r2)) 

district_df <- district_df %>%
  ungroup() %>%
  group_by(country_name) %>%
  dplyr::mutate(r2 = cor(truth, prediction)^2) %>%
  ungroup() %>%
  dplyr::mutate(country_name = fct_reorder(country_name, -r2))

# Load/prep accuracy data ------------------------------------------------------
acc_df <- readRDS(file.path(data_dir, SURVEY_NAME, "FinalData", "pov_estimation_results",
                            "accuracy_appended.Rds"))

acc_df <- acc_df %>%
  dplyr::filter(level_change %in% "changes") %>%
  dplyr::mutate(feature_type_clean = feature_type_clean %>%
           str_replace_all("Daytime Imagery: Avg. & Std. Dev.",
                           "Daytime Imagery:\nAvg. & Std. Dev."))

# Boxplots: By Training Sample -------------------------------------------------
p_boxplot_tsample <- acc_df %>%
  dplyr::filter(feature_type %in% "all_changes",
                target_var_dep %in% "pca_allvars") %>%
  ggplot(aes(x = reorder(estimation_type_clean, r2, FUN = mean, .desc =TRUE),
             y = r2)) +
  geom_half_boxplot(errorbar.draw = FALSE, center = TRUE,
                    fill = FILL_COLOR) +
  stat_summary(fun = median, geom = "text", col = "black",     # Add text to plot
               vjust = -0.2, hjust = 0.5, aes(label = paste(round(..y.., digits = 2)))) +
  stat_summary(fun = p75, geom = "text", col = "black",     # Add text to plot
               vjust = -0.2, hjust = -0.05, aes(label = paste(round(..y.., digits = 2)))) +
  stat_summary(fun = max, geom = "text", col = "firebrick3",    
               vjust = -0.2, aes(label = paste(round(..y.., digits = 2)))) +
  stat_summary(fun = min, hjust = 0.05, geom = "text", col = "firebrick3",   
               vjust = -0.2, aes(label = paste(round(..y.., digits = 2)))) +
  labs(x = NULL,
       y = expression(r^2),
       title = "A. Performance by training sample type [cluster]") +
  theme_classic() +
  theme(legend.position = "none",
        axis.text.y = element_text(face = "bold"),
        plot.title = element_text(face = "bold"),
        plot.title.position = "plot") +  
  coord_flip() 

# Bar Chart using Best Training Type by Unit -----------------------------------
best_df <- bind_rows(
  cluster_df %>%
    distinct(country_name, .keep_all = T) %>%
    dplyr::mutate(type = "Cluster"),
  
  district_df %>%
    distinct(country_name, .keep_all = T) %>%
    dplyr::mutate(type = "District")
)

best_clean_df <- best_df %>%
  dplyr::mutate(r2_cat = case_when(
    r2 < 0.1 ~ "0 - 0.1",
    r2 >= 0.1 & r2 < 0.2 ~ "0.1 - 0.2",
    r2 >= 0.2 & r2 < 0.3 ~ "0.2 - 0.3",
    r2 >= 0.3 & r2 < 0.4 ~ "0.3 - 0.4",
    r2 >= 0.4 & r2 < 0.5 ~ "0.4 - 0.5",
    r2 >= 0.5 & r2 < 0.6 ~ "0.5 - 0.6",
    r2 >= 0.6 & r2 < 0.7 ~ "0.6 - 0.7",
    r2 >= 0.7 & r2 < 0.8 ~ "0.7 - 0.8",
    r2 >= 0.8 & r2 < 0.9 ~ "0.8 - 0.9"
  )) %>%
  group_by(r2_cat,
           type) %>%
  dplyr::summarise(n = n()) %>%
  ungroup() %>%
  tidyr::complete(r2_cat, type, fill = list(n = 0)) %>%
  group_by(type) %>%
  dplyr::mutate(prop = n / sum(n)) %>%
  ungroup() %>%
  dplyr::mutate(text = paste0(n, " (", round(prop,2)*100,"%)"))

p_cluster_adm <- best_clean_df %>%
  ggplot(aes(y = r2_cat,
             x = n,
             fill = type)) +
  geom_col(position = "dodge",
           color = "black") +
  geom_text(aes(label = text,
                x = n + 2),
            position = position_dodge(width = .9)) +
  labs(x = "N Countries",
       y = expression(r^2),
       fill = "Unit",
       title = "C. Performance by unit") +
  scale_fill_manual(values = c("lightblue1",
                               "tan1"),
                    guide = guide_legend(reverse = TRUE)) +
  xlim(0, 32) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.title.y = element_text(angle = 0, vjust = 0.5),
        plot.title = element_text(face = "bold"),
        legend.position = c(0.75,0.75),
        plot.title.position = "plot") 

# Boxplots: By Feature Set -----------------------------------------------------
p_boxplot_feature <- acc_df %>%
  dplyr::filter(estimation_type %in% "global_country_pred",
                target_var_dep %in% "pca_allvars") %>%
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
  stat_summary(fun = min, hjust = 0.05, geom = "text", col = "firebrick3",    
               vjust = -0.2, aes(label = paste(round(..y.., digits = 2)))) +
  labs(x = NULL,
       y = expression(r^2),
       title = "B. Performance using different features [cluster]") +
  #scale_y_continuous(limits = c(0,0.6)) +
  theme_classic() +
  theme(legend.position = "none",
        axis.text.y = element_text(face = "bold"),
        plot.title = element_text(face = "bold"),
        plot.title.position = "plot") +  
  coord_flip() 

# Arrange and export ----------------------------------------------------------------------
p_left <- ggarrange(p_boxplot_tsample,
                    p_cluster_adm, 
                    nrow = 2)

p <- ggarrange(p_left, p_boxplot_feature,
               nrow = 1)

ggsave(p,
       filename = file.path(figures_global_dir, "changes_results.png"),
       height = 6,
       width = 12)

## Stats
d_r2 <- best_df %>%
  dplyr::filter(type == "District") %>%
  pull(r2)

mean(d_r2 > 0.25)
sum(d_r2 > 0.25)

mean(d_r2 > 0.5)
sum(d_r2 > 0.5)
