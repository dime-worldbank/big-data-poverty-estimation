# Extracts values of landsat to BISP coordinates

SURVEY_NAME <- "DHS"
satellite <- "sentinel"

# Load Survey Data -------------------------------------------------------------
df <- readRDS(file.path(secure_file_path, "Data", SURVEY_NAME,  "FinalData - PII", "GPS_uid_crosswalk.Rds"))

if(SURVEY_NAME %in% "DHS"){
  df <- df[df$most_recent_survey %in% T,]
}

# Create Grid ------------------------------------------------------------------
write.csv(df, "~/Desktop/dhs.csv")



