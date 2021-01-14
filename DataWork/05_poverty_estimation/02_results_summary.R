# Results

results_binary_df <- file.path(project_file_path, 'Data', 'Poverty Estimation Results', 'binary_classification', 'individual_files') %>%
  list.files(full.names = T) %>%
  lapply(read.csv) %>%
  bind_rows() %>%
  mutate(f1 = 2*(precision_score * recall_score) / (precision_score + recall_score)) %>%
  arrange(desc(f1)) %>%
  dplyr::select(-X)

results_cont_df <- file.path(project_file_path, 'Data', 'Poverty Estimation Results', 'continuous_classification', 'individual_files') %>%
  list.files(full.names = T) %>%
  lapply(read.csv) %>%
  bind_rows() %>%
  dplyr::select(-X)

pred_df <- read.csv(file.path(project_file_path, 'Data', 'Poverty Estimation Results', 
                              'continuous_classification', 'predicted_values', 
                              paste0("results_", 18, ".csv")))

cor(pred_df$y, pred_df$y_pred_17) %>% sqrt()

plot(pred_df$y, pred_df$y_pred_17)

