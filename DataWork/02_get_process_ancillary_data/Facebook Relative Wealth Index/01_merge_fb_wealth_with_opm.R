# Locations to Scrape for Facebook Data

# Facebook limits the number of API requests that can be made during specific
# time intervals. To reduce the number of API calls that need to be made, we
# reduce the number of locations where we extract data. In short, if households 
# are close together, we group them together and use the average coordiantes.

# The script identifies the locations to scrapes, and creates a crosswalk of these
# location areas back to the original househld survey data.

set.seed(42)

# Load Data --------------------------------------------------------------------
opm_coords <- read.csv(file.path(secure_file_path, "Data", "BISP", "FinalData - PII", "GPS_uid_crosswalk.csv"),
                       stringsAsFactors = F)

opm_coords$uid <- opm_coords$uid %>% as.numeric()

# Some coordinates are bad; remove those
opm_coords <- opm_coords[(opm_coords$latitude < 37) & (opm_coords$latitude > 23),]
opm_coords <- opm_coords[(opm_coords$longitude < 81) & (opm_coords$longitude > 65),]

coordinates(opm_coords) <- ~longitude+latitude

# Load FB Data -----------------------------------------------------------------
fb_df <- read.csv(file.path(project_file_path, "Data", "Facebook Relative Wealth Index", 
                            "RawData", "ind_pak_relative_wealth_index.csv"))

coordinates(fb_df) <- ~longitude+latitude
fb_df <- fb_df %>% crop(extent(opm_coords))

# Nearest Neighbor -------------------------------------------------------------
opm_coords <- as.data.frame(opm_coords)
fb_df <- as.data.frame(fb_df)

closest <- nn2(fb_df[,c("latitude", "longitude")], 
               opm_coords[,c("latitude", "longitude")], 
               k = 1, 
               searchtype = "radius", 
               radius = 1)
closest <- sapply(closest, cbind) %>% as_tibble

opm_coords$fb_id <- closest$nn.idx

fb_df <- fb_df %>%
  dplyr::mutate(fb_id = 1:n()) %>%
  dplyr::select(fb_id, rwi, error) %>%
  dplyr::rename(fb_rwi = rwi,
                fb_rwi_error = error)

opm_coords <- merge(opm_coords, fb_df, by = "fb_id", all.x=T, all.y=F)

# Export -----------------------------------------------------------------------
opm_coords <- opm_coords %>%
  dplyr::select(uid, fb_rwi, fb_rwi_error)

write.csv(opm_coords, file.path(project_file_path, "Data", "OPM", "FinalData", "Individual Datasets", 
                                           "fb_relative_wealth.csv"),
          row.names = F)

saveRDS(opm_coords, file.path(project_file_path, "Data", "OPM", "FinalData", "Individual Datasets", 
                                "fb_relative_wealth.Rds"))





