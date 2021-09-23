# Clean survey data

df <- readRDS(file.path(data_dir, SURVEY_NAME, "FinalData", "survey_alldata.Rds"))

saveRDS(df, file.path(data_dir, SURVEY_NAME, "FinalData", "survey_alldata_clean.Rds"))
write.csv(df, file.path(data_dir, SURVEY_NAME, "FinalData", "survey_alldata_clean.csv"),
          row.names = F)


