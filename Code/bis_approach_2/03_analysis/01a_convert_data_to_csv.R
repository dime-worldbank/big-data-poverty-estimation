# 01_convert_data_to_csv
# 
# DESCRIPTION:
# Satellite data by household is in .rds format, and BISP household data is in
# Stata15 format. Meanwhile, ML work will be done in Python.
# This script converts the rds and Stata data to csv files.

# Setup
library(here)
library(tidyverse)
library(readstata13)


# Key variables 
#SAT_INPUT_PATH <- here('Data', 'FinalData', 'BISP', 'bisp_satellite_data.Rds')
SAT_INPUT_PATH <- file.path(final_data_file_path, "BISP", "bisp_satellite_data_buffer_1km.Rds")
SAT_OUTPUT_PATH <- file.path(final_data_file_path, 'Outputs for Analysis TEMP', 'bisp_satellite_data.csv')

#BISP_INPUT_PATH <- here('Data', 'RawData', 'BISP', 'bisp_combined_plist.dta')
BISP_INPUT_PATH <- file.path(raw_data_file_path, "BISP","BISP - Deidentified", "bisp_combined_plist.dta")
BISP_OUTPUT_PATH <- file.path(final_data_file_path, 'Outputs for Analysis TEMP', 'bisp_hh_income.csv')

# Import, export satellite data
sat_df <- readRDS(SAT_INPUT_PATH)
sat_df %>% write_csv(SAT_OUTPUT_PATH)

# Import, summarize, export household income data
bisp_df <- read.dta13(BISP_INPUT_PATH)
bisp_df <- bisp_df %>%
  dplyr::select(uid, period, CQ12) %>%
  mutate(period = case_when(period == 0 ~ 2011,
                            period == 1 ~ 2013,
                            period == 2 ~ 2014, 
                            period == 3 ~ 2016
  )) %>%   
  group_by(uid, period) %>%
  summarize(hh_inc = sum(CQ12, na.rm = TRUE))

bisp_df %>% write_csv(BISP_OUTPUT_PATH)
