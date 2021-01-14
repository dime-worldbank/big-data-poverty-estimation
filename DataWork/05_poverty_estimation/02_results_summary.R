# Results


results_df <- file.path(project_file_path, 'Data', 'Poverty Estimation Results', 'binary_classification', 'individual_files') %>%
  list.files(full.names = T) %>%
  lapply(read.csv) %>%
  bind_rows()

results_df$f1 <- 2 * (results_df$precision_score * results_df$recall_score) / (results_df$precision_score + results_df$recall_score)


