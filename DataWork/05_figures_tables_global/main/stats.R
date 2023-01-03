# Basic Stats

SURVEY_NAME <- "DHS_OLD"

# Load data --------------------------------------------------------------------
df <- readRDS(file.path(data_dir, SURVEY_NAME, "FinalData", "Merged Datasets", 
                        "survey_alldata_clean.Rds"))

# Sample size ------------------------------------------------------------------
df$country_code %>% unique() %>% length()
df %>% nrow()


