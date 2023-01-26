# CNN Results

# Load data --------------------------------------------------------------------
pred_dir <- file.path(data_dir, "DHS", "FinalData", "Individual Datasets", "cnn_predictions")

s2_viirs_undert_rgb <- read_csv(file.path(pred_dir, "predictions_s2_viirs_underiaTrue_b_rgb.csv"))
s2_viirs_undert_bu <- read_csv(file.path(pred_dir, "predictions_landsat_viirs_underiaTrue_b_bu.csv"))
s2_viirs_undert_ndvi <- read_csv(file.path(pred_dir, "predictions_landsat_viirs_underiaTrue_b_ndvi.csv"))
landsat_viirs_undert_rgb <- read_csv(file.path(pred_dir, "predictions_landsat_viirs_underiaTrue_b_rgb.csv"))
landsat_viirs_undert_bu <- read_csv(file.path(pred_dir, "predictions_s2_viirs_underiaTrue_b_bu.csv"))
landsat_viirs_undert_ndvi <- read_csv(file.path(pred_dir, "predictions_s2_viirs_underiaTrue_b_ndvi.csv"))

# Figures ----------------------------------------------------------------------
s2_viirs_undert_bu %>%
  ggplot(aes(x = predictions)) +
  geom_histogram() +
  facet_wrap(~true_values)
