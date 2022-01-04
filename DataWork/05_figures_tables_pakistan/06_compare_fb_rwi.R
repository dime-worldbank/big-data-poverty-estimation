# Compare Facebook RWI

# TODO: Make table out of this

# Load data --------------------------------------------------------------------
#pakpoints_df <- readRDS(file.path(data_dir, "PAK_POINTS", 
#                                  "FinalData", "Merged Datasets", "survey_alldata_clean_predictions.Rds"))

dhs_df <- readRDS(file.path(data_dir, "DHS", "FinalData", "Merged Datasets", 
                            "survey_alldata_clean_predictions.Rds"))

# Prep data --------------------------------------------------------------------
dhs_df <- dhs_df %>%
  dplyr::filter(country_code %in% "PK",
                !is.na(fb_rwi)) 

# Compute correlations ---------------------------------------------------------
cor_df <- dhs_df %>%
  dplyr::summarise(r2_estimate = cor(wealth_index_score, predict_wealth_index_score_best)^2,
                   r2_fbrwi = cor(wealth_index_score, fb_rwi)^2)

cor_ur_df <- dhs_df %>%
  group_by(urban_rural) %>%
  dplyr::summarise(r2_estimate = cor(wealth_index_score, predict_wealth_index_score_best)^2,
                   r2_fbrwi = cor(wealth_index_score, fb_rwi)^2,
                   wealth_index_score_sd = sd(wealth_index_score),
                   wealth_index_score_mean = mean(wealth_index_score))

cor_adm2_df <- dhs_df %>%
  group_by(NAME_1) %>%
  dplyr::summarise(r2_estimate = cor(wealth_index_score, predict_wealth_index_score_best)^2,
                   r2_fbrwi = cor(wealth_index_score, fb_rwi)^2,
                   wealth_index_score_sd = sd(wealth_index_score),
                   wealth_index_score_mean = mean(wealth_index_score))

cor_ur_df
cor_df
cor_adm2_df

cor_adm2_df %>%
ggplot() +
  geom_point(aes(x = r2_estimate,
                 y = wealth_index_score_sd))

