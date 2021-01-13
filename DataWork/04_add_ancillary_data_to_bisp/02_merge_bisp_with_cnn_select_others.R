# CNN Models

library(glmnet)
set.seed(42)

# Load Data --------------------------------------------------------------------
## BISP Data
bisp_df <- readRDS(file.path(project_file_path, "Data", "BISP", 
                             "FinalData", "Individual Datasets", "bisp_socioeconomic.Rds"))

## Ancillary Data
cnn_df <- read.csv(file.path(bisp_indiv_files_dir, "bisp_cnn_features_all_Nbands3_nNtlBins3_minNTLbinCount16861.csv"))
viirs_df <- read.csv(file.path(bisp_indiv_files_dir, "bisp_viirs.csv"))
landsat_df <- read.csv(file.path(bisp_indiv_files_dir, "bisp_viirs.csv"))

# Merge ------------------------------------------------------------------------
viirs_df <- viirs_df %>%
  dplyr::select(uid,
                viirs_buff1km_year2014_spatialMEAN_monthlyMEAN, 
                viirs_buff5km_year2014_spatialMEAN_monthlyMEAN)

cnn_df <- cnn_df %>%
  dplyr::select(-X)

bisp_df <- merge(bisp_df, viirs_df, by = c("uid"), all.x=T, all.y=F)
bisp_df <- merge(bisp_df, cnn_df, by = c("uid"), all.x=T, all.y=F)

# Export -----------------------------------------------------------------------
saveRDS(bisp_df, file.path(project_file_path, "Data", "BISP", "FinalData", "Merged Datasets", "cnn_merge.Rds"))
write.csv(bisp_df, file.path(project_file_path, "Data", "BISP", "FinalData", "Merged Datasets", "cnn_merge.csv"), row.names=F)



