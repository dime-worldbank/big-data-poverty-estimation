# CNN Results Table

# https://stackoverflow.com/questions/33081702/accuracy-precision-and-recall-for-multi-class-model

# Load Data --------------------------------------------------------------------
results_cat_df <- read.csv(file.path(project_file_path, "Data", "CNN", "Nbands3_nNtlBins3_minNTLbinCount16861", "cnn_predictions_truth_values.csv"))
results_cont_df <- read.csv(file.path(project_file_path, "Data", "CNN", "Nbands3_nNtlBins3_minNTLbinCount16861", "cnn_predictions_continuous_truth_values.csv"))




# Prep Data --------------------------------------------------------------------
r <- ml_test(results_cat_df$predY,
             results_cat_df$testY)
r$precision
r$recall
r$accuracy

results_cont_df %>%
  ggplot() +
  geom_point(aes(x = testY, y = predY))

cor.test(results_cont_df$testY, results_cont_df$predY)



