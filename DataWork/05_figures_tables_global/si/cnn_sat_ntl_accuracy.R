# Accuracy of CNN Models Predicting NTL

# Load data --------------------------------------------------------------------
rgb_df <- readRDS(file.path(data_dir,
                            SURVEY_NAME,
                            "FinalData",
                            "Individual Datasets",
                            "cnn_features",
                            "cnn_predictions_s2_rgb.Rds"))

ndvi_df <- readRDS(file.path(data_dir,
                            SURVEY_NAME,
                            "FinalData",
                            "Individual Datasets",
                            "cnn_features",
                            "cnn_predictions_s2_ndvi.Rds"))

bu_df <- readRDS(file.path(data_dir,
                            SURVEY_NAME,
                            "FinalData",
                            "Individual Datasets",
                            "cnn_features",
                            "cnn_predictions_s2_bu.Rds"))

rgb_df %>%
  mutate(correct = predictions == true_values) %>%
  pull(correct) %>%
  mean()

ndvi_df %>%
  mutate(correct = predictions == true_values) %>%
  pull(correct) %>%
  mean()

bu_df %>%
  mutate(correct = predictions == true_values) %>%
  pull(correct) %>%
  mean()


head(rgb_df)

rgb_df %>%
  ggplot() +
  geom_histogram(aes(x = predictions)) +
  facet_wrap(~true_values)




