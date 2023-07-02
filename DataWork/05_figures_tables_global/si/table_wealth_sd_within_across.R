# Check Variation

# Load data --------------------------------------------------------------------
df <- readRDS(file.path(data_dir, "DHS", "FinalData", "Merged Datasets", 
                        "survey_alldata_clean.Rds"))

# Aggregate --------------------------------------------------------------------
district_df <- df %>%
  dplyr::filter(most_recent_survey %in% T) %>%
  group_by(gadm_uid, country_name) %>%
  dplyr::summarise(pca_district_avg = mean(pca_allvars_mr),
                   pca_district_sd = sd(pca_allvars_mr)) %>%
  ungroup() %>%
  dplyr::filter(!is.na(pca_district_sd))

country_df <- district_df %>%
  group_by(country_name) %>%
  dplyr::summarise(across_district_pca_sd = sd(pca_district_avg),
                   average_within_district_sd = mean(pca_district_sd)) %>%
  ungroup() %>%
  mutate(difference = across_district_pca_sd - average_within_district_sd,
         tex = paste0(country_name, " & ", 
                      round(across_district_pca_sd, 2), " & ",
                      round(average_within_district_sd, 2), " \\\\ \n ")) %>%
  arrange(country_name)

table(country_df$difference > 0)
mean(country_df$difference > 0)

# Table ------------------------------------------------------------------------
sink(file.path(tables_global_dir, "within_across_pca_sd.tex"))

cat("\\begin{tabular}{lcc} \n")
cat("\\hline \n")
cat("Country & Std. Dev. Wealth & Average Within District \\\\ \n ")
cat("        & Across Districts & Wealth Std. Dev. \\\\ \n ")
cat("\\hline \n")
for(i in 1:nrow(country_df)) cat(country_df$tex[i])
cat("\\hline \n")
cat("\\end{tabular}")

sink()




