# Merge Data and Prepare Final Dataset
# Produce _WIDE and _LONG dataset

# Load Data --------------------------------------------------------------------
bisp_satdata_df <- readRDS(file.path(final_data_file_path, "BISP", "Merged Datasets", "bisp_socioeconomic_satellite_panel_full.Rds"))

bisp_satdata_df <- bisp_satdata_df[bisp_satdata_df$survey_round %in% c(1,3),]
bisp_satdata_df <- bisp_satdata_df %>%
  group_by(uid) %>%
  mutate(N_uid = n())
bisp_satdata_df <- bisp_satdata_df[bisp_satdata_df$N_uid %in% 2,]

# First Difference
bisp_satdata_df <- bisp_satdata_df[order(bisp_satdata_df$survey_round),]
bisp_satdata_df_firstdiff <- bisp_satdata_df %>%
  group_by(uid) %>%
  mutate_at(vars(income_last_month:viirs_spatialsd_monthlysd_buff_10km), funs(.-lag(.)))

bisp_satdata_df_firstdiff <- bisp_satdata_df_firstdiff[bisp_satdata_df_firstdiff$survey_round %in% 3,]

# Export -----------------------------------------------------------------------
write.csv(bisp_satdata_df_firstdiff, file.path(final_data_file_path, "BISP", "Merged Datasets", "bisp_socioeconomic_satellite_firstdiff_r13.csv"), row.names=F)


