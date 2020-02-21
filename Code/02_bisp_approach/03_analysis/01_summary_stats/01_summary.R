# Summary Stats





bisp_satdata_df <- readRDS(file.path(final_data_file_path, "BISP", "Merged Datasets", "bisp_socioeconomic_satellite_panel_full.Rds"))

bisp_satdata_df$survey_round %>% table()



lm(income_last_year ~  viirs_spatialmean_monthlymean_buff_2km, data=bisp_satdata_df) %>% summary()

bisp_satdata_df$income_last_year






