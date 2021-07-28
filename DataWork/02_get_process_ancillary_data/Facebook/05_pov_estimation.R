# Poverty Estimation

# Load Data --------------------------------------------------------------------
SURVEY_NAME <- "DHS"

survey_pov <- readRDS(file.path(data_dir, SURVEY_NAME, "FinalData", "Individual Datasets", "survey_socioeconomic.Rds"))
fb_prop_df <- readRDS(file.path(data_dir, SURVEY_NAME, "FinalData", "Individual Datasets", "facebook_marketing_dau_mau_prop.Rds"))

survey_pov <- survey_pov %>%
  left_join(fb_prop_df, by = "uid") 


plot(survey_pov$wealth_index_score, survey_pov$asset_pca_1)

results_df <- read.csv("~/Desktop/results_temp.csv")
pred_vals <- read.csv("~/Desktop/pred_temp.csv")

lm(y ~ y_pred_9, data = pred_vals) %>% summary()

pred_vals %>%
  ggplot() +
  geom_point(aes(x = y,
                 y = y_pred_9)) +
  labs(x = "True",
       y = "Predicted")



