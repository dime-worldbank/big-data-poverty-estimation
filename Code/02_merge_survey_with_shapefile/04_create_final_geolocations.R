# Clean NSER Survey

# Load and Clean Data ----------------------------------------------------------
survey_df <- readRDS(file.path(final_data_file_path, "UC with NSER Data", "individual_files", "nsersurvey_withdata.Rds"))
survey_hdx_shp_names <- readRDS(file.path(final_data_file_path, "UC with NSER Data", "individual_files", "nsersurvey_matched_hdx_shapefile.Rds"))
survey_googlecoords <- readRDS(file.path(final_data_file_path, "UC with NSER Data", "individual_files", "nsersurvey_matched_google_coords.Rds"))

survey_df <- merge(survey_df, survey_hdx_shp_names, by="id", all=T)
survey_df <- merge(survey_df, survey_googlecoords, by="id", all=T)
