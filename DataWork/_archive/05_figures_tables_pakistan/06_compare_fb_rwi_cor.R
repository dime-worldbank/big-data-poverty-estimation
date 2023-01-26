# Compare Facebook RWI

# TODO: Make table out of this

# Load data --------------------------------------------------------------------
#pakpoints_df <- readRDS(file.path(data_dir, "PAK_POINTS", 
#                                  "FinalData", "Merged Datasets", "survey_alldata_clean_predictions.Rds"))

dhs_df <- readRDS(file.path(data_dir, "DHS", "FinalData", "Merged Datasets", 
                            "survey_alldata_clean_predictions.Rds"))

# Prep data --------------------------------------------------------------------
dhs_df <- dhs_df %>%
  dplyr::filter(country_code %in% "PK")

orig_N <- nrow(dhs_df)

dhs_df <- dhs_df %>%
  dplyr::filter(!is.na(fb_rwi))

new_N <- nrow(dhs_df)

orig_N - new_N

# dhs_long_df <- dhs_df %>%
#   dplyr::select(uid, urban_rural, wealth_index_score, predict_wealth_index_score_best, fb_rwi) %>%
#   pivot_longer(-c(uid, urban_rural, wealth_index_score))

# Compute correlations ---------------------------------------------------------
# dhs_long_df %>%
#   ggplot() +
#   geom_point(aes(x = wealth_index_score,
#                  y = value)) +
#   facet_wrap(urban_rural~name, scales = "free_y")

cor_df <- dhs_df %>%
  dplyr::summarise(r2_estimate = cor(wealth_index_score, predict_wealth_index_score_best)^2,
                   r2_fbrwi = cor(wealth_index_score, fb_rwi)^2) %>%
  dplyr::mutate(tex = paste0("All", " & ",
                             round(r2_estimate,2), " & ",
                             round(r2_fbrwi, 2), " \\\\ \n"))

cor_ur_df <- dhs_df %>%
  group_by(urban_rural) %>%
  dplyr::summarise(r2_estimate = cor(wealth_index_score, predict_wealth_index_score_best)^2,
                   r2_fbrwi = cor(wealth_index_score, fb_rwi)^2,
                   wealth_index_score_sd = sd(wealth_index_score),
                   wealth_index_score_mean = mean(wealth_index_score),
                   N = n()) %>%
  dplyr::mutate(urban_rural = case_when(
    urban_rural == "R" ~ "Rural",
    urban_rural == "U" ~ "Urban"
  )) %>%
  dplyr::mutate(tex = paste0(urban_rural, " & ",
                            round(r2_estimate,2), " & ",
                            round(r2_fbrwi, 2), " \\\\ \n"))

# Table ------------------------------------------------------------------------
sink(file.path(tables_pak_dir, "compare_rwi.tex"))
cat("\\begin{tabular}{lcc} \n")
cat("\\hline \n")
cat(" & Model Estimate & FB RWI \\\\ \n")
cat("\\hline \n")
cat(cor_df$tex[1])
cat(cor_ur_df$tex[cor_ur_df$urban_rural %in% "Urban"])
cat(cor_ur_df$tex[cor_ur_df$urban_rural %in% "Rural"])
cat("\\hline \n")
cat("\\end{tabular}")
sink()
