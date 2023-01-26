# Facebook Correlation

SURVEY_NAME <- "DHS"

# Load Data --------------------------------------------------------------------
survey_df <- readRDS(file.path(dhs_dir, "FinalData", "Individual Datasets", "survey_socioeconomic.Rds"))
fb_df <- readRDS(file.path(data_dir, SURVEY_NAME, "FinalData", "Individual Datasets", "facebook_marketing_dau_mau_prop.Rds"))
param_df <- read.csv(file.path(data_dir, SURVEY_NAME, "FinalData", "Individual Datasets", "facebook_marketing_parameters_clean.csv"), stringsAsFactors = F)

data_df <- left_join(survey_df,
                     fb_df,
                     by = "uid") %>%
  dplyr::filter(!is.na(estimate_mau_2),
                !is.na(asset_pca_1))

# Correlations -----------------------------------------------------------------
data_df <- data_df %>%
  dplyr::select(country_code, asset_pca_1, wealth_index_score, contains("_mau_")) %>%
  pivot_longer(cols = -c(country_code, asset_pca_1, wealth_index_score))

cor_df <- data_df %>%
  group_by(country_code, name) %>%
  dplyr::summarise(asset_pca_1 = cor(asset_pca_1, value),
                   wealth_index_score = cor(wealth_index_score, value))



head(survey_df)

a <- survey_df[survey_df$within_country_fold %in% "BD_4",]
leaflet() %>%
  addTiles() %>%
  addCircles(data = a,
             lat = ~latitude,
             lng = ~longitude)




