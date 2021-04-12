# Locations to Scrape for Facebook Data

# Facebook limits the number of API requests that can be made during specific
# time intervals. To reduce the number of API calls that need to be made, we
# reduce the number of locations where we extract data. In short, if households 
# are close together, we group them together and use the average coordiantes.

# The script identifies the locations to scrapes, and creates a crosswalk of these
# location areas back to the original househld survey data.

set.seed(42)

# Load HH Coordinates ----------------------------------------------------------
opm_coords <- read.csv(file.path(secure_file_path, "Data", "BISP", "FinalData - PII", "GPS_uid_crosswalk.csv"),
                        stringsAsFactors = F)

opm_coords$uid <- opm_coords$uid %>% as.numeric()

# Some coordinates are bad; remove those
opm_coords <- opm_coords[(opm_coords$latitude < 37) & (opm_coords$latitude > 23),]
opm_coords <- opm_coords[(opm_coords$longitude < 81) & (opm_coords$longitude > 65),]

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
saveRDS(opm_cluster_crosswalk, file.path(project_file_path, "Data", "Facebook", "FinalData", "locations_to_scrape", "opm_cluster_crosswalk.Rds"))
write.csv(opm_cluster_crosswalk, file.path(project_file_path, "Data", "Facebook", "FinalData", "locations_to_scrape", "opm_cluster_crosswalk.csv"),
          row.names = F)

saveRDS(cluster_df, file.path(project_file_path, "Data", "Facebook", "FinalData", "locations_to_scrape", "cluster_locations.Rds"))
write.csv(cluster_df, file.path(project_file_path, "Data", "Facebook", "FinalData", "locations_to_scrape", "cluster_locations.csv"),
          row.names = F)





