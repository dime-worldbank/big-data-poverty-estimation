# Changes: Stats

# Load data --------------------------------------------------------------------
cluster_df <- readRDS(file.path(data_dir, SURVEY_NAME, "FinalData", "Merged Datasets",
                                "survey_alldata_clean_changes_cluster_predictions.Rds"))

# N ----------------------------------------------------------------------------
nrow(cluster_df)
