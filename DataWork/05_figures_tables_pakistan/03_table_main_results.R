# Main Results Tables

# Load data --------------------------------------------------------------------
results_df <- readRDS(file.path(data_dir, SURVEY_NAME, "FinalData", "pov_estimation_results",
                                "accuracy_appended.Rds"))
results_df <- results_df %>%
  dplyr::filter(country_name %in% "Pakistan",
                !(estimation_type %in% "best")) %>%
  dplyr::mutate(r2 = cor^2)

# Table: estimation_type & target_var ------------------------------------------
results_sum_df <- results_df %>%
  dplyr::filter(feature_type %in% "all") %>%
  pivot_wider(id_cols = target_var_clean,
              names_from = estimation_type,
              values_from = r2) %>%
  mutate_if(is.numeric, ~round(., 2) %>% 
              as.character() %>%
              tidyr::replace_na("")) %>%
  dplyr::mutate(tex = paste(target_var_clean, " & ",
                            within_country_cv, " & ",
                            same_continent, " & ",
                            other_continents, " & ",
                            global_country_pred, " \\\\ \n"))

sink(file.path(tables_pak_dir, "main_results.tex"))
cat("\\begin{tabular}{l cccc} \n")
cat("\\hline \n")
cat("         & \\multicolumn{4}{c}{Training Sample} \\\\ \n")
cat("\\cline{2-5} \n")
cat("Variable & Within & Same & Other & \\\\ \n")
cat("         & Country & Continent & Continents & Global \\\\ \n")
cat("\\hline \n")

for(i in 1:nrow(results_sum_df)) cat(results_sum_df$tex[i])

cat("\\hline \n")
cat("\\end{tabular}")
sink()

# Figure: feature_type ---------------------------------------------------------
p <- results_df %>%
  dplyr::filter(estimation_type %in% "within_country_cv",
                target_var %in% "wealth_index_score") %>%
  ggplot(aes(y = reorder(feature_type_clean, r2, FUN = mean, .desc =TRUE),
             x = r2,
             xmin = 0,
             xmax = r2,
             label = round(r2, 2))) +
  #geom_point(position = position_dodge2(width = DODGE_WIDTH)) +
  #geom_linerange(position = position_dodge2(width = DODGE_WIDTH)) +
  geom_col() +
  geom_text(aes(x = r2 + 0.03)) +
  labs(x = expression(r^2),
       y = NULL,
       color = "Wealth\nVariable",
       title = "B. Results by features used in training") +
  theme_minimal() +
  theme(axis.text.y = element_text(color = "black"),
        plot.title = element_text(face = "bold"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  scale_x_continuous(limits = c(0, 0.55))

ggsave(p, 
       filename = file.path(figures_pak_dir, "r2_features.png"),
       height = 2.5,
       width = 5)

# Table: target_var & feature_type ---------------------------------------------
results_df %>%
  dplyr::filter(estimation_type %in% "within_country_cv") %>%
  pivot_wider(id_cols = feature_type_clean,
              names_from = target_var,
              values_from = r2) 

DODGE_WIDTH = 0.5
results_df %>%
  dplyr::filter(estimation_type %in% "within_country_cv") %>%
  ggplot(aes(y = reorder(feature_type_clean, r2, FUN = mean, .desc =TRUE),
             x = r2,
             xmin = 0,
             xmax = r2,
             color = target_var_clean)) +
  geom_point(position = position_dodge2(width = DODGE_WIDTH)) +
  geom_linerange(position = position_dodge2(width = DODGE_WIDTH)) +
  labs(x = expression(r^2),
       y = NULL,
       color = "Wealth\nVariable",
       title = "A. Results by target variable & features used in training") +
  theme_minimal() +
  theme(axis.text.y = element_text(color = "black"),
        plot.title = element_text(face = "bold")) +
  scale_color_brewer(palette = "Set1")

# Table: estimation_type & feature_type ----------------------------------------
results_df %>%
  dplyr::filter(target_var %in% "pca_allvars") %>%
  pivot_wider(id_cols = feature_type_clean,
              names_from = estimation_type,
              values_from = r2) 

DODGE_WIDTH = 0.5
results_df %>%
  dplyr::filter(target_var %in% "pca_allvars") %>%
  ggplot(aes(y = reorder(feature_type_clean, r2, FUN = mean, .desc =TRUE),
             x = r2,
             xmin = 0,
             xmax = r2,
             color = estimation_type_clean)) +
  geom_point(position = position_dodge2(width = DODGE_WIDTH)) +
  geom_linerange(position = position_dodge2(width = DODGE_WIDTH)) +
  #geom_col(position = position_dodge2(width = DODGE_WIDTH)) +
  labs(x = expression(r^2),
       y = NULL,
       color = "Training\nSample",
       title = "B. Results by training sample & features used in training") +
  theme_minimal() +
  theme(axis.text.y = element_text(color = "black"),
        plot.title = element_text(face = "bold")) +
  scale_color_brewer(palette = "Set1")
