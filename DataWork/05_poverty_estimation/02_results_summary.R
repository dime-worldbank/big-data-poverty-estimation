# Results

results_df <- file.path(project_file_path, 'Data', 'Poverty Estimation Results', 'binary_classification', 'individual_files') %>%
  list.files(full.names = T) %>%
  lapply(read.csv) %>%
  bind_rows() %>%
  mutate(f1 = 2*(precision_score * recall_score) / (precision_score + recall_score)) %>%
  arrange(desc(f1)) %>%
  dplyr::select(-X)


results_df$f1 %>% head()



