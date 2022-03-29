# Table showing R2 across Different Data Subsets for Training

# Load data --------------------------------------------------------------------
acc_df <- readRDS(file.path(data_dir, SURVEY_NAME, "FinalData", "pov_estimation_results",
                            "accuracy_appended.Rds"))

acc_df <- acc_df %>%
  dplyr::filter(target_var %in% c("pca_allvars",
                                  "pca_nonphysicalvars",
                                  "pca_physicalvars"),
                feature_type %in% "all")

# Prep Data: Country Summary Stat ----------------------------------------------
country_df <- acc_df %>%
  group_by(estimation_type, feature_type, target_var, country) %>%
  dplyr::summarise(N = sum(N_fold),
                   cor = cor_country[1]) %>% # This repeats across folds
  dplyr::mutate(r2 = cor^2)

country_sum_df <- country_df %>%
  group_by(target_var, estimation_type) %>%
  dplyr::summarise(r2_min = min(r2),
                   r2_q10 = quantile(r2, probs = 0.1),
                   r2_mean = mean(r2),
                   r2_median = median(r2),
                   r2_q90 = quantile(r2, probs = 0.9),
                   r2_max = max(r2)) 

# Prep Data: Pooled ------------------------------------------------------------
pred_df <- file.path(data_dir, "DHS", "FinalData", "pov_estimation_results", "predictions") %>%
  list.files(pattern = "_all.Rds", 
             full.names = T) %>%
  str_subset("pca_allvars_all|pca_nonphysicalvars_all|pca_physicalvars_all") %>%
  map_df(readRDS)

pooled_df <- pred_df %>%
  dplyr::mutate(estimation_type = case_when(
    estimation_type %>% str_detect("continent_") & estimation_type %>% str_detect("country_pred") ~ "Same Continent",
    estimation_type %in% "global_country_pred" ~ "Global",
    estimation_type %in% "within_country_cv" ~ "Within Country",
    estimation_type %in% "continent" ~ "Other Continents"
  )) %>%
  group_by(estimation_type, target_var) %>%
  dplyr::summarise(r2_pooled = cor(truth, prediction)^2) 

# Merge, Prep ------------------------------------------------------------------
df <- country_sum_df %>%
  left_join(pooled_df, by = c("target_var", "estimation_type")) %>%
  dplyr::mutate_if(is.numeric, ~ round(. , digits = 3)) %>%
  dplyr::mutate(tex = paste(estimation_type,
                            r2_pooled,
                            r2_min,
                            r2_q10,
                            r2_median,
                            r2_mean,
                            r2_q90,
                            r2_max,
                            sep = " & ") %>%
                  paste(" \\\\ \n"))

# Table ------------------------------------------------------------------------
sink(file.path(tables_dir, "r2_across_data_subsets.tex"))
cat("\\begin{tabular}{l | c | cccccc} \n")
cat("\\hline \n")
cat("Type & Pooled R$^2$ & \\multicolumn{6}{|c}{Within Country R$^2$} \\\\ \n")
cat("     &              & Min & Q1 & Median & Mean & Q90 & Max \\\\ \n")

cat("\\hline \n")
cat("\\multicolumn{8}{l}{\\bf Global Wealth Index} \\\\ \n ")
df_i <- df[df$target_var %in% "pca_allvars",]
for(i in 1:nrow(df_i)) cat(df_i$tex[i])

cat("\\hline \n")
cat("\\multicolumn{8}{l}{\\bf Household Attribute Index} \\\\ \n ")
df_i <- df[df$target_var %in% "pca_physicalvars",]
for(i in 1:nrow(df_i)) cat(df_i$tex[i])

cat("\\hline \n")
cat("\\multicolumn{8}{l}{\\bf Non-Household Attribute Index} \\\\ \n ")
df_i <- df[df$target_var %in% "pca_nonphysicalvars",]
for(i in 1:nrow(df_i)) cat(df_i$tex[i])

cat("\\hline \n")
cat("\\end{tabular}")
sink()





