# Clean OPM Data

# Clean Oxford Policy Management (OPM) survey data. Create Household Level 
# dataframe with relevant socioeconomic variables.

set.seed(42)

# Load Data --------------------------------------------------------------------
bisp_agg_df <- readRDS(file.path(opm_dir, "FinalData", "Individual Datasets", "survey_socioeconomic.Rds"))

# Export -----------------------------------------------------------------------
