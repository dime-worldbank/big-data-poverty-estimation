# CNN Results Table

# https://stackoverflow.com/questions/33081702/accuracy-precision-and-recall-for-multi-class-model

# Load Data --------------------------------------------------------------------
results_cat_df <- read.csv(file.path(gdrive_cnn_file_path, "Nbands3_nNtlBins3_minNTLbinCount16861",  "cnn_predictions_truth_values_2014.csv"))
results_cont_df <- read.csv(file.path(gdrive_cnn_file_path, "Nbands3_nNtlBins3_minNTLbinCount16861",  "cnn_predictions_continuous_truth_values_2014.csv"))

# Prep Data --------------------------------------------------------------------
r <- ml_test(results_cat_df$predY,
             results_cat_df$testY)
r$precision
r$recall
r$accuracy

results_cont_df %>%
  #head(100) %>%
  ggplot() +
  geom_point(aes(x = testY, y = predY),
             size = .05, 
             alpha = 1) +
  labs(x = "Nighttime lights radiance",
       y = "Predicted\nnighttime\nlights\nradiance",
       caption = "Nighttime lights values are logged") +
  theme_ipsum() +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.5),
        plot.caption = element_text(size = 6)) +
  ggsave(file.path(figures_file_path, "ntlpred_continuous_2014.png"),
         height = 3, width = 4)




