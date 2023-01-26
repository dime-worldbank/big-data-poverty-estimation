# Merge data with Survey Data

SURVEY_NAME <- "DHS"

# Load Data --------------------------------------------------------------------
INDV_DATA_DIR <- file.path(data_dir, SURVEY_NAME, "FinalData", "Individual Datasets")

## Survey Data
survey_df <- readRDS(file.path(INDV_DATA_DIR, "survey_socioeconomic.Rds"))

## OSM
osm_df <- readRDS(file.path(INDV_DATA_DIR, "osm.Rds"))

## Facebook
fb_n_df <- readRDS(file.path(INDV_DATA_DIR, "facebook_marketing_dau_mau.Rds"))
fb_prop_df <- readRDS(file.path(INDV_DATA_DIR, "facebook_marketing_dau_mau_prop.Rds"))

## Landsat (sum stats)
l8_df <- read.csv(file.path(INDV_DATA_DIR, "survey_l8.csv"), stringsAsFactors = F)

## VIIRS (sum stats)
viirs_df <- read.csv(file.path(INDV_DATA_DIR, "survey_viirs.csv"), stringsAsFactors = F)

## World Pop
#worldpop_5km_df <- read.csv(file.path(INDV_DATA_DIR, "survey_worldpop_5km.csv"), stringsAsFactors = F)
worldpop_10km_df <- read.csv(file.path(INDV_DATA_DIR, "survey_worldpop_10km.csv"), stringsAsFactors = F)

# Prep specific datasets -------------------------------------------------------

#### Facebook
# (1) Version that is mau divided by worldpop
# (2) Remove DAU
# (3) Rename so starts with "fb_"

fb_prop_wp_df <- fb_n_df %>%
  left_join(worldpop_10km_df, by = "uid") %>%
  dplyr::select(-contains("dau")) %>% 
  mutate_at(vars(contains("mau")), ~ . / worldpop_sum) %>% 
  dplyr::select(-worldpop_sum) %>%
  mutate_if(is.numeric, ~ ifelse(. > 1, 1, .)) %>% 
  rename_at(vars(-uid), ~ paste0("fb_prop_wp_", .)) 

fb_n_df <- fb_n_df %>%
  dplyr::select(-contains("dau")) %>% 
  rename_at(vars(-uid), ~ paste0("fb_", .))

fb_prop_df <- fb_prop_df %>%
  dplyr::select(-contains("dau")) %>% 
  rename_at(vars(-uid), ~ paste0("fb_prop_mau_", .))

# Merge Data -------------------------------------------------------------------
survey_clean_df <- survey_df %>%
  left_join(osm_df, by = "uid") %>%
  left_join(fb_n_df, by = "uid") %>%
  left_join(fb_prop_df, by = "uid") %>%
  left_join(fb_prop_wp_df, by = "uid") %>%
  left_join(l8_df, by = "uid") %>%
  left_join(viirs_df, by = "uid") %>%
  left_join(worldpop_10km_df, by = "uid") 

# Adjust variables -------------------------------------------------------------
log_p1 <- function(x) log(x + 1)

survey_clean_df <- survey_clean_df %>%
  # Some OSM variables are "units" objects; convert to numeric
  dplyr::mutate_at(vars(contains("osm")), as.numeric) %>%
  
  # Log skewed variables
  dplyr::mutate_at(vars(viirs_avg_rad,
                        worldpop_sum,
                        contains("osm")),
                   log_p1)

# Subset -----------------------------------------------------------------------
# Remove if NA values

survey_clean_df <- survey_clean_df %>%
  dplyr::filter(!is.na(fb_estimate_mau_1))

# Export -----------------------------------------------------------------------
saveRDS(survey_clean_df, file.path(data_dir, SURVEY_NAME, "FinalData", "Merged Datasets", "survey_alldata.Rds"))
write.csv(survey_clean_df, file.path(data_dir, SURVEY_NAME, "FinalData", "Merged Datasets", "survey_alldata.csv"),
          row.names = F)

