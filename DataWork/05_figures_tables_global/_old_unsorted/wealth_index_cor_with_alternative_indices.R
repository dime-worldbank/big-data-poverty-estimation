# Compare DHS Wealth Index vs Computed Global Wealth Index

# Load Data --------------------------------------------------------------------
survey_df <- readRDS(file.path(dhs_dir, "FinalData", "Individual Datasets", "survey_socioeconomic.Rds"))

country_cor <- survey_df %>%
  dplyr::select(country_code, wealth_index_score, pca_allvars, pca_physicalvars, pca_nonphysicalvars) %>%
  pivot_longer(cols = -c(country_code, wealth_index_score)) %>%
  group_by(country_code, name) %>%
  dplyr::summarise(cor = cor(value, wealth_index_score)) %>%
  ungroup() %>%
  group_by(name) %>%
  dplyr::summarise_at(vars(cor), list(mean = mean, 
                                      min = min,
                                      max = max)) %>%
  dplyr::mutate(vars_include = case_when(
    name == "pca_allvars" ~ "All of the below",
    name == "pca_physicalvars" ~ "Household floor, roof and wall quality; Access 
    to electricity",
    name == "pca_nonphysicalvars" ~ "Asset ownership (TV, fridge, motorbike, car); 
    Time to get drinking water; Flush toilet connected to sewer; N people sleeping
    per bedroom; Education level"
  )) %>%
  mutate(vars_include = vars_include %>% 
           str_replace_all("\\n", "") %>%
           str_squish()) %>%
  dplyr::mutate(name_clean = case_when(
    name == "pca_allvars" ~ "Global Wealth Index",
    name == "pca_physicalvars" ~ "Household Attribute Index",
    name == "pca_nonphysicalvars" ~ "Non-Household ~~~~~~ Attribute Index"
  )) %>%
  mutate(tex = paste(name_clean, " & ",
                     round(mean, 3), " & ", 
                     round(min, 3), " & ", 
                     round(max, 3), " & ", 
                     vars_include, "\\\\ \\hline \n "))

sink(file.path(tables_dir, "indices_cor_dhs.tex"))
cat("\\begin{tabular}{p{3.2cm}|ccc|p{6.4cm}} \n")
cat("\\hline \n")
cat("Index & \\multicolumn{3}{c|}{Within Country Correlation} & Components \\\\ \n")
cat("      & \\multicolumn{3}{c|}{with DHS Index} &  \\\\ \n")
cat("      & Mean & Min & Max & \\\\ \n")
cat("\\hline \n")
for(i in 1:nrow(country_cor)) cat(country_cor$tex[i])
cat("\\end{tabular} ")
sink()

cor(survey_df$pca_allvars, survey_df$pca_nonphysicalvars)
cor(survey_df$pca_allvars, survey_df$pca_physicalvars)
cor(survey_df$pca_nonphysicalvars, survey_df$pca_physicalvars)






