# Summary Stats

#### Load Data
bisp_satdata_df <- readRDS(file.path(final_data_file_path, "BISP", "Merged Datasets", "bisp_socioeconomic_satellite_panel_full.Rds"))
bisp_satdata_analysissample_df <- readRDS(file.path(final_data_file_path, "BISP", "Merged Datasets", "bisp_socioeconomic_satellite_panel_full_satPovNAsRemoved.Rds"))

#### Summarise Data
# Summarise full BISP Sample
bisp_summary_df <- bisp_satdata_df %>%
  group_by(survey_round, year) %>%
  summarise(N_hh = n(),
            pscores_avg = mean(pscores, na.rm=T),
            poor_prop = mean(pscores_poor, na.rm=T)) %>%
  ungroup()

# Summarise BISP Analysis Sample
bisp_analysissample_summary_df <- bisp_satdata_analysissample_df %>%
  group_by(survey_round, year) %>%
  summarise(N_hh_as = n(),
            pscores_avg_as = mean(pscores, na.rm=T),
            poor_prop_as = mean(pscores_poor, na.rm=T)) %>%
  ungroup()

# Merge
bisp_summary_df <- merge(bisp_summary_df, bisp_analysissample_summary_df, by=c("survey_round", "year"))

# Make Table -------------------------------------------------------------------
#### Round select variables
for(var in names(bisp_summary_df)[3:8]){
  bisp_summary_df[[var]] <- bisp_summary_df[[var]] %>% round(2)
}

#### Create latex code
bisp_summary_df$latex <- apply(bisp_summary_df, 1, paste, collapse=" & ") %>% paste(" \\\\ \n")

#### Make Table
sink(file.path(tables_file_path, "bisp_N_poverty_sumstat.tex"))
cat("\\begin{tabular}{cc|ccc|ccc} \n")
cat("\\hline \n")
cat("       &      & \\multicolumn{3}{c|}{Full Sample} & \\multicolumn{3}{c}{Sample with Coordinates} \\\\ \n")
cat("\\hline \n")
cat("Survey & Year & Number of  & Average       & Proportion & Number of  & Average       & Proportion \\\\ \n")
cat("Round  &      & Households & Poverty Score & Poor       & Households & Poverty Score & Poor \\\\ \n")
cat("\\hline \n")

for(i in 1:nrow(bisp_summary_df)) cat(bisp_summary_df$latex[i])

cat("\\hline \n")
cat("\\end{tabular} \n")
sink()

