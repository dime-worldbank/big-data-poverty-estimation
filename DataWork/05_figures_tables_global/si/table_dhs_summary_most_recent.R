# DHS Summary Table

# Load Data --------------------------------------------------------------------
df <- readRDS(file.path(data_dir, SURVEY_NAME, "FinalData", "Merged Datasets", "survey_alldata_clean.Rds"))

df_sum <- df %>%
  group_by(country_name) %>%
  filter(most_recent_survey %in% T) %>%
  ungroup() %>%
  
  group_by(country_name, year) %>%
  dplyr::summarise(N = n(),
                   
                   pca_allvars_mr_mean = mean(pca_allvars_mr, na.rm = T),
                   pca_nonphysicalvars_mr_mean = mean(pca_nonphysicalvars_mr, na.rm = T),
                   pca_physicalvars_mr_mean = mean(pca_physicalvars_mr, na.rm = T),
                   
                   pca_allvars_mr_sd = sd(pca_allvars, na.rm = T),
                   pca_nonphysicalvars_mr_sd = sd(pca_nonphysicalvars_mr, na.rm = T),
                   pca_physicalvars_mr_sd = sd(pca_physicalvars_mr, na.rm = T)) %>%
  dplyr::mutate_if(is.numeric, ~ round(., digits = 2)) %>%
  dplyr::mutate(tex = paste0(country_name, " & ", year, " & ", N, " & ", 
                            pca_allvars_mr_mean, " (", pca_allvars_mr_sd, ") & ",
                            pca_nonphysicalvars_mr_mean, " (", pca_nonphysicalvars_mr_sd, ") & ",
                            pca_physicalvars_mr_mean, " (", pca_physicalvars_mr_sd, ")  ",
                            " \\\\ \n "))

sink(file.path(tables_global_dir, "dhs_summary_table.tex"))
cat("\\begin{tabular}{l cc | ccc} \n")
cat("\\hline \n")
cat("Country & DHS Year & N Obs & Global Index & HH Index & Non-HH Index \\\\ \n")
cat("        &          &       & Mean (sd)    & Mean (sd) & Mean (sd) \\\\ \n")
cat("\\hline \n")

for(i in 1:nrow(df_sum)) cat(df_sum$tex[i])

cat("\\hline \n")
cat("\\end{tabular}")
sink()

