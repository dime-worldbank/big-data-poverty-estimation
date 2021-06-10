# Locations to Scrape for Facebook Data

# Facebook limits the number of API requests that can be made during specific
# time intervals. To reduce the number of API calls that need to be made, we
# reduce the number of locations where we extract data. In short, if households 
# are close together, we group them together and use the average coordiantes.

# The script identifies the locations to scrapes, and creates a crosswalk of these
# location areas back to the original househld survey data.

set.seed(42)

# Load HH Coordinates ----------------------------------------------------------
opm_coords <- readRDS(SURVEY_COORDS_PATH)

# Add projected coords
coordinates(opm_coords) <- ~longitude+latitude
crs(opm_coords) <- CRS("+init=epsg:4326")
opm_coords <- opm_coords %>% spTransform(PAK_UTM_PROJ) %>% as.data.frame()

# Make Clusters ----------------------------------------------------------------
opm_coords_dist <- opm_coords[,c("latitude", "longitude")] %>% dist()
opm_coords$cluster_id <- hclust(opm_coords_dist, method = "ward.D2") %>%
  cutree(h = 1000)

# CRS to WGS84 -----------------------------------------------------------------
coordinates(opm_coords) <- ~longitude+latitude
crs(opm_coords) <- CRS(PAK_UTM_PROJ)
opm_coords <- opm_coords %>% spTransform("+init=epsg:4326") %>% as.data.frame()

# Make Final Dataframes --------------------------------------------------------
opm_cluster_crosswalk <- opm_coords %>%
  dplyr::select(uid, cluster_id)

cluster_df <- opm_coords %>%
  group_by(cluster_id) %>%
  summarise_at(vars(c(latitude, longitude)), mean)

# Export -----------------------------------------------------------------------
saveRDS(opm_cluster_crosswalk,   file.path(project_file_path, "Data", SURVEY_NAME, "FinalData", "Individual Datasets", "fb_mau_cluster_crosswalk.Rds"))
write.csv(opm_cluster_crosswalk, file.path(project_file_path, "Data", SURVEY_NAME, "FinalData", "Individual Datasets", "fb_mau_cluster_crosswalk.csv"),
          row.names = F)

saveRDS(cluster_df,              file.path(project_file_path, "Data", SURVEY_NAME, "FinalData", "Individual Datasets", "fb_mau_cluster_locations.Rds"))
write.csv(cluster_df,            file.path(project_file_path, "Data", SURVEY_NAME, "FinalData", "Individual Datasets", "fb_mau_cluster_locations.csv"),
          row.names = F)



