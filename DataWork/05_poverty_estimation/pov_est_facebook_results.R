# Pakistan Poverty Estimation Results

tables_dir <- file.path(project_file_path, 'Outputs', 'Results', 'Tables')
figures_dir <- file.path(project_file_path, 'Outputs', 'Results', 'Figures')

# Load Data --------------------------------------------------------------------
survey_df <- readRDS(file.path(dhs_dir, "FinalData", "Individual Datasets", "survey_socioeconomic.Rds"))

results_df <- read.csv(file.path(data_dir, "DHS", "FinalData", "results", "results_fbonly.csv"))
ypred_df <- fread(file.path(data_dir, "DHS", "FinalData", "results", "ypred_fbonly.csv"))

# Results Table ----------------------------------------------------------------

survey_sum_df <- survey_df %>%
  dplyr::group_by(country_code) %>%
  dplyr::summarise(survey_year = min(year),
                   survey_N = n())

results_table_df <- results_df %>%
  dplyr::rename(country_code = country) %>%
  left_join(survey_sum_df, by = "country_code") %>%
  dplyr::mutate(country = case_when(
    country_code == "BD" ~ "Bangladesh",
    country_code == "IA" ~ "India",
    country_code == "KH" ~ "Cambodia",
    country_code == "KY" ~ "Kyrgyzstan",
    country_code == "MM" ~ "Myanmar",
    country_code == "NP" ~ "Nepal",
    country_code == "PH" ~ "Philippines",
    country_code == "PK" ~ "Pakistan",
    country_code == "TJ" ~ "Tajikistan",
    country_code == "TL" ~ "Timor Leste"
  )) %>%
  dplyr::select(r2_score_val, est_type, country, survey_year, survey_N) %>%
  pivot_wider(values_from = r2_score_val,
              names_from = est_type) %>%
  dplyr::arrange(country) %>%
  dplyr::mutate_at(vars(within_country, other_countries, india), round, 3) %>%
  dplyr::mutate(tex = paste(country, " & ", 
                            survey_year, " & ", 
                            survey_N, " & ", 
                            within_country, " & ", 
                            other_countries, " & ", 
                            india, " \\\\ \n "))

sink(file.path(tables_dir, "fb_results.tex"))
cat("\\begin{tabular}{lll | lll} \n")
cat("\\hline \n")
cat("        &        &        & \\multicolumn{3}{c}{R$^2$ - Training Set} \\\\ \n")
cat("Country & Survey & Survey & Within & All Other & India \\\\ \n ")
cat("        & Year   & N      & Country & Countries &     \\\\ \n ")
cat("\\hline \n")

for(i in 1:nrow(results_table_df)) cat(results_table_df$tex[i])

cat("\\hline \n")
cat("\\end{tabular}")
sink()

# Results Figure ---------------------------------------------------------------

#results_lim_df <- results_df %>%
#  dplyr::select(country, est_type, target, r2_score_val)

ypred_clean_df <- ypred_df %>%
  dplyr::mutate(country_name = case_when(
    country == "BD" ~ "Bangladesh",
    country == "IA" ~ "India",
    country == "KH" ~ "Cambodia",
    country == "KY" ~ "Kyrgyzstan",
    country == "MM" ~ "Myanmar",
    country == "NP" ~ "Nepal",
    country == "PH" ~ "Philippines",
    country == "PK" ~ "Pakistan",
    country == "TJ" ~ "Tajikistan",
    country == "TL" ~ "Timor Leste"
  )) #%>%
#left_join(results_lim_df, by = c("country", "est_type", "target"))

my.formula <- y ~ x

for(est_type_i in c("within_country",
                    "other_countries",
                    "india")){
  
  p <- ypred_clean_df %>%
    dplyr::filter(est_type %in% est_type_i) %>%
    ggplot() +
    geom_point(aes(x = y,
                   y = y_pred),
               alpha = 0.5,
               size = 0.2) +
    stat_poly_eq(formula = my.formula, 
                 aes(x = y,
                     y = y_pred,
                     label = paste(..rr.label.., sep = "~~~")), 
                 parse = TRUE,
                 size = 3) +   
    labs(x = "Poverty Score: True",
         y = "Poverty\nScore:\nPredicted") +
    theme_classic() +
    theme(strip.text = element_text(face = "bold"),
          strip.background = element_blank(),
          axis.title.y = element_text(angle = 0, vjust = 0.5)) +
    facet_wrap(~country_name,
               nrow = 2) 
  ggsave(p, 
         filename = file.path(figures_dir, paste0("fb_",est_type_i,".png")), 
         height = 5,
         width = 8.5)
  
}

