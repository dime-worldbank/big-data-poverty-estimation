# Clean NSER Survey

# Load and Clean Data ----------------------------------------------------------
survey_uc_df <- read.csv(file.path(project_file_path, "Data", "RawData", "NSER", "Union Council Level Dataset","nser_data_uc.csv"))

survey_uc_df <- survey_uc_df %>%
  dplyr::rename(district_name_original = district_name) %>%
  dplyr::rename(tehsil_name_original = tehsil_name) %>%
  dplyr::rename(unioncouncil_name_original = unioncouncil_name)

survey_uc_df$district_name_original <- survey_uc_df$district_name_original %>% as.character %>% tolower
survey_uc_df$tehsil_name_original <- survey_uc_df$tehsil_name_original %>% as.character %>% tolower
survey_uc_df$unioncouncil_name_original <- survey_uc_df$unioncouncil_name_original %>% as.character %>% tolower

survey_uc_df$id <- 1:nrow(survey_uc_df)

survey_uc_df_onlynamevars <- survey_uc_df %>%
  dplyr::select(id, unioncouncil_name_original, tehsil_name_original, district_name_original)

survey_uc_df <- survey_uc_df %>%
  dplyr::select(-tehsil_name_howfoundmatch, -unioncouncil_name_howfoundmatch,
                -tehsil_name_gislayer, -district_name_gislayer, -unioncouncil_name_gislayer, -district_name_howfoundmatch)

# Export -----------------------------------------------------------------------
saveRDS(survey_uc_df, file.path(final_data_file_path, "UC with NSER Data", "individual_files", "nsersurvey_withdata.Rds"))
saveRDS(survey_uc_df_onlynamevars, file.path(final_data_file_path, "UC with NSER Data", "individual_files", "nsersurvey_blank.Rds"))

