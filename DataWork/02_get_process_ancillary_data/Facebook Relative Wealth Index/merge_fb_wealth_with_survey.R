# Locations to Scrape for Facebook Data

# Facebook limits the number of API requests that can be made during specific
# time intervals. To reduce the number of API calls that need to be made, we
# reduce the number of locations where we extract data. In short, if households 
# are close together, we group them together and use the average coordiantes.

# The script identifies the locations to scrapes, and creates a crosswalk of these
# location areas back to the original househld survey data.

set.seed(42)

# Load Data --------------------------------------------------------------------
survey_coords <- readRDS(SURVEY_COORDS_PATH)

coordinates(survey_coords) <- ~longitude + latitude
crs(survey_coords) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

# Load FB Data -----------------------------------------------------------------
fb_df <- read.csv(file.path(project_file_path, "Data", "Facebook Relative Wealth Index", 
                            "RawData", "ind_pak_relative_wealth_index.csv"))

coordinates(fb_df) <- ~longitude+latitude
fb_df <- fb_df %>% crop(extent(survey_coords))

# Nearest Neighbor -------------------------------------------------------------
survey_coords <- as.data.frame(survey_coords)
fb_df <- as.data.frame(fb_df)

closest <- nn2(fb_df[,c("latitude", "longitude")], 
               survey_coords[,c("latitude", "longitude")], 
               k = 1, 
               searchtype = "radius", 
               radius = 1)
closest <- sapply(closest, cbind) %>% as_tibble

survey_coords$fb_id <- closest$nn.idx

fb_df <- fb_df %>%
  dplyr::mutate(fb_id = 1:n()) %>%
  dplyr::select(fb_id, rwi, error) %>%
  dplyr::rename(fb_rwi = rwi,
                fb_rwi_error = error)

survey_coords <- merge(survey_coords, fb_df, by = "fb_id", all.x=T, all.y=F)

# Export -----------------------------------------------------------------------
survey_coords <- survey_coords %>%
  dplyr::select(uid, fb_rwi, fb_rwi_error)

write.csv(survey_coords, file.path(project_file_path, "Data", SURVEY_NAME, "FinalData", "Individual Datasets", 
                                           "fb_relative_wealth.csv"),
          row.names = F)

saveRDS(survey_coords, file.path(project_file_path, "Data", SURVEY_NAME, "FinalData", "Individual Datasets", 
                                "fb_relative_wealth.Rds"))





