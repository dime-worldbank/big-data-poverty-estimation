# Pooled R2: Africa, Urban vs Rural

# Load data --------------------------------------------------------------------
df <- readRDS(file.path(data_dir, "DHS", "FinalData", "Merged Datasets", 
                        "survey_alldata_clean.Rds"))

df <- readRDS(file.path(data_dir, "DHS", "FinalData", "Merged Datasets", 
                        "survey_alldata_clean_predictions.Rds"))


df %>%
  dplyr::filter(most_recent_survey == T) %>%
  group_by(continent_adj, urban_rural) %>%
  dplyr::summarise(r2 = cor(pca_allvars_mr, predict_pca_allvars_mr_global_country_pred_all)^2)

df %>%
  dplyr::filter(most_recent_survey == T) %>%
  group_by(continent_adj) %>%
  dplyr::summarise(r2 = cor(pca_allvars_mr, predict_pca_allvars_mr_global_country_pred_all)^2)

