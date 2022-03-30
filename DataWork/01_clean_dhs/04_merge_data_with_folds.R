# Merge DHS Data with Within Country Folds

# Load data --------------------------------------------------------------------
folds_df <- readRDS(file.path(dhs_dir, "FinalData", "Individual Datasets", "survey_fold.Rds"))
survey_df <- readRDS(file.path(dhs_dir, "FinalData", "Individual Datasets", "survey_socioeconomic_varconstructed_tmp.Rds"))

# Merge data -------------------------------------------------------------------
survey_folds_df <- survey_df %>%
  left_join(folds_df, by = "uid")

# Export data ------------------------------------------------------------------
saveRDS(survey_folds_df,
        file.path(dhs_dir, "FinalData", "Individual Datasets", "survey_socioeconomic.Rds"))

write.csv(survey_folds_df,
        file.path(dhs_dir, "FinalData", "Individual Datasets", "survey_socioeconomic.csv"),
        row.names = F)


