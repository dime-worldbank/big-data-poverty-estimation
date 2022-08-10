# DHS Summary Table

# Load Data --------------------------------------------------------------------
df <- readRDS(file.path(data_dir, SURVEY_NAME, "FinalData", "Merged Datasets", "survey_alldata_clean.Rds"))

df_sum <- df %>%
  group_by(country_name) %>%
  mutate(year_min = min(year),
         year_max = max(year)) %>%
  ungroup() %>%
  filter(year_min != year_max) %>%
  group_by(country_name) %>%
  dplyr::summarise(N_min = sum(year == year_min),
                   N_max = sum(year == year_max),
                   
                   year_min = year_min[1],
                   year_max = year_max[2],
                   
                   pca_allvars_mean_min = mean(pca_allvars[year == year_min], na.rm = T),
                   pca_allvars_mean_max = mean(pca_allvars[year == year_max], na.rm = T),
                   
                   pca_allvars_sd_min = sd(pca_allvars[year == year_min], na.rm = T),
                   pca_allvars_sd_max = sd(pca_allvars[year == year_max], na.rm = T)) %>%
  dplyr::mutate_if(is.numeric, ~ round(., digits = 2)) %>%
  dplyr::mutate(tex = paste0(country_name, 
                             " & ", year_min, " & ", N_min, " & ", 
                             pca_allvars_mean_min, " (", pca_allvars_sd_min, ") ",
                             " & ", year_max, " & ", N_max, " & ", 
                             pca_allvars_mean_max, " (", pca_allvars_sd_max, ")  ",
                             " \\\\ \n "))

sink(file.path(tables_global_dir, "dhs_summary_table_changes.tex"))
cat("\\begin{tabular}{l | ccc | ccc} \n")
cat("\\hline \n")
cat("Country & \\multicolumn{3}{c}{1st Survey Since 2000} & \\multicolumn{3}{c}{Latest Survey} \\\\ \n")
cat("        & Year & N & Asset Index              & Year & N & Asset Index    \\\\ \n")
cat("        &      &   & Mean (sd)                &      &   & Mean (sd)      \\\\ \n")

cat("\\hline \n")

for(i in 1:nrow(df_sum)) cat(df_sum$tex[i])

cat("\\hline \n")
cat("\\end{tabular}")
sink()


