# Main Results

FILL_COLOR <- "gray80"

# Load data --------------------------------------------------------------------
results_df <- readRDS(file.path(data_dir, SURVEY_NAME, "FinalData", "pov_estimation_results",
                                "accuracy_appended_bestparam.Rds"))

results_df <- results_df %>%
  dplyr::filter(level_change %in% "levels",
                !(target_var %in% "pca_allvars"))

# Load/prep survey data --------------------------------------------------------
cluster_df <- readRDS(file.path(data_dir, SURVEY_NAME, "FinalData", "Merged Datasets",
                                "survey_alldata_clean_predictions.Rds"))

cluster_df <- cluster_df %>%
  dplyr::filter(most_recent_survey == T) 

district_df <- cluster_df %>%
  group_by(continent_adj, country_code, country_name, gadm_uid) %>%
  #summarise_if(is.numeric, mean) %>%
  dplyr::summarise(pca_allvars_mr = mean(pca_allvars_mr),
                   predict_pca_allvars_mr_best = mean(predict_pca_allvars_mr_best)) %>%
  ungroup()

## Add correlation
cluster_df <- cluster_df %>%
  group_by(country_name) %>%
  mutate(r2 = cor(pca_allvars_mr, predict_pca_allvars_mr_best)^2) %>%
  ungroup() %>%
  dplyr::mutate(country_name = fct_reorder(country_name, -r2)) 

district_df <- district_df %>%
  group_by(country_name) %>%
  mutate(r2 = cor(pca_allvars_mr, predict_pca_allvars_mr_best)^2) %>%
  ungroup() %>%
  dplyr::mutate(country_name = fct_reorder(country_name, -r2)) 


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
# p_targetvar <- results_df %>%
#   dplyr::filter(feature_type %in% "all",
#                 estimation_type %in% "within_country_cv") %>%
#   ggplot(aes(x = reorder(target_var_clean, r2, FUN = median, .desc =TRUE),
#              y = r2)) +
#   geom_half_boxplot(errorbar.draw = FALSE, center = TRUE,
#                     fill = FILL_COLOR) +
#   #geom_half_point(transformation = position_jitter(width = 0.05, height = 0.1)) +
#   stat_summary(fun = median, geom = "text", col = "black",     # Add text to plot
#                vjust = -0.2, aes(label = paste(round(..y.., digits = 2)))) +
#   stat_summary(fun = max, geom = "text", col = "firebrick3",    
#                vjust = -0.2, aes(label = paste(round(..y.., digits = 2)))) +
#   labs(x = NULL,
#        y = expression(r^2),
#        title = "B. Performance by outcome variable") +
#   scale_y_continuous(limits = c(0,1)) +
#   theme_classic() +
#   theme(legend.position = "none",
#         axis.text.y = element_text(face = "bold"),
#         plot.title = element_text(face = "bold"),
#         plot.title.position = "plot") +
#   coord_flip() 

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

# Bar Chart using Best Training Type by Unit -----------------------------------
best_df <- bind_rows(
  cluster_df %>%
    distinct(country_name, .keep_all = T) %>%
    mutate(type = "Village"),
  
  district_df %>%
    distinct(country_name, .keep_all = T) %>%
    mutate(type = "District")
)

best_sum_df <- best_df %>%
  mutate(r2_cat = case_when(
    r2 < 0.1 ~ "0 - 0.1",
    r2 >= 0.1 & r2 < 0.2 ~ "0.1 - 0.2",
    r2 >= 0.2 & r2 < 0.3 ~ "0.2 - 0.3",
    r2 >= 0.3 & r2 < 0.4 ~ "0.3 - 0.4",
    r2 >= 0.4 & r2 < 0.5 ~ "0.4 - 0.5",
    r2 >= 0.5 & r2 < 0.6 ~ "0.5 - 0.6",
    r2 >= 0.6 & r2 < 0.7 ~ "0.6 - 0.7",
    r2 >= 0.7 & r2 < 0.8 ~ "0.7 - 0.8",
    r2 >= 0.8 & r2 < 0.9 ~ "0.8 - 0.9",
    r2 >= 0.9 & r2 < 1 ~ "0.9 - 1"
  )) %>%
  group_by(r2_cat,
           type) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  tidyr::complete(r2_cat, type, fill = list(n = 0)) %>%
  group_by(type) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup() %>%
  mutate(text = paste0(n, " (", round(prop,2)*100,"%)")) %>%
  group_by(type) %>%
  mutate(n_sum = cumsum(n),
         prop_sum = cumsum(prop)) %>%
  ungroup()

p_cluster_adm <- best_sum_df %>%
  ggplot(aes(y = r2_cat,
             x = n,
             fill = type)) +
  geom_col(position = "dodge",
           color = "black") +
  geom_text(aes(label = text,
                x = n + 1.75),
            size = 2.5,
            position = position_dodge(width = .9)) +
  labs(x = "N Countries",
       y = expression(r^2),
       fill = "Unit",
       title = "B. Performance by unit") +
  scale_fill_manual(values = c("lightblue1",
                               "tan1"),
                    guide = guide_legend(reverse = TRUE)) +
  xlim(0, 30) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.title.y = element_text(angle = 0, vjust = 0.5),
        plot.title = element_text(face = "bold"),
        legend.position = c(0.9,0.9),
        plot.title.position = "plot") 

# Arrange ----------------------------------------------------------------------
p_l <- ggarrange(p_trainsample,
                 p_cluster_adm,
                 nrow = 2,
                 heights = c(0.45, 0.55))

p <- ggarrange(p_l, 
               p_feature,
               nrow = 1)

ggsave(p, 
       filename = file.path(figures_global_dir, "performence_country_avg_types.png"),
       height = 6,
       width = 10)




