# Locations to Scrape for Facebook Data

# Facebook limits the number of API requests that can be made during specific
# time intervals. To reduce the number of API calls that need to be made, we
# reduce the number of locations where we extract data. In short, if households 
# are close together, we group them together and use the average coordiantes.

# The script identifies the locations to scrapes, and creates a crosswalk of these
# location areas back to the original househld survey data.

set.seed(42)

# Load data --------------------------------------------------------------------
fb_df <- readRDS(file.path(fb_rwi_dir, "FinalData", "fb_rwi.Rds"))
survey_df <- readRDS(file.path(data_dir, SURVEY_NAME, "FinalData", "Individual Datasets", "survey_socioeconomic.Rds"))

# coordinates(fb_rwi_df) <- ~longitude+latitude
# crs(fb_rwi_df) <- CRS("+init=epsg:4326")
# 
# coordinates(survey_df) <- ~longitude+latitude
# crs(fb_rwi_df) <- CRS("+init=epsg:4326")

# Nearest Neighbor -------------------------------------------------------------
#survey_coords <- as.data.frame(survey_coords)
#fb_df <- as.data.frame(fb_df)

closest <- nn2(fb_df[,c("latitude", "longitude")], 
               survey_df[,c("latitude", "longitude")], 
               k = 1, 
               searchtype = "radius", 
               radius = 1)
closest <- sapply(closest, cbind) %>% as_tibble

survey_df$fb_id <- closest$nn.idx

fb_df <- fb_df %>%
  dplyr::mutate(fb_id = 1:n()) %>%
  dplyr::select(fb_id, rwi, error, latitude, longitude) %>%
  dplyr::rename(fb_rwi = rwi,
                fb_rwi_error = error,
                fb_rwi_latitude = latitude,
                fb_rwi_longitude = longitude)

survey_df <- merge(survey_df, fb_df, by = "fb_id", all.x=T, all.y=F)

survey_df$dist_fbrwi_km <- distGeo(p1 = as.matrix(survey_df[,c("longitude","latitude")]),
                                   p2 = as.matrix(survey_df[,c("fb_rwi_longitude","fb_rwi_latitude")])) / 1000

survey_df$fb_rwi[survey_df$dist_fbrwi_km >= 2] <- NA
survey_df$fb_rwi_error[survey_df$dist_fbrwi_km >= 2] <- NA

# Export -----------------------------------------------------------------------
survey_df <- survey_df %>%
  dplyr::select(uid, fb_rwi, fb_rwi_error)

saveRDS(survey_df, file.path(data_dir, SURVEY_NAME, "FinalData", "Individual Datasets", 
                             "fb_relative_wealth.Rds"))

