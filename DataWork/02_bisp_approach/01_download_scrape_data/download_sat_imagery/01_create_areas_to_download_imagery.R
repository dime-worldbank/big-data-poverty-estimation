# Create Clusters of HH IDs

# Create clusters where, for each cluster, we will download satellite imagery
# Cluster datasets should not be stored in Dropbox. In combination, they could
# potentially be used to identify households (eg, if the cluster size is 1)

cluster_search_radius_meters <- 4000
max_cluster_size_meters <- 5000

# Load Data --------------------------------------------------------------------
bisp_coords <- read_dta(file.path(bisp_geocodes_file_path, "GPS_uid_crosswalk.dta"))
bisp_coords <- bisp_coords[!is.na(bisp_coords$GPSN),]

# Convert Coords to Lat/Lon ----------------------------------------------------
bisp_coords$latitude <- get_lat_lon(bisp_coords$GPSN)
bisp_coords$longitude <- get_lat_lon(bisp_coords$GPSE)

# Restrict to households in/near Pakistan
bisp_coords <- bisp_coords[(bisp_coords$latitude < 37) & (bisp_coords$latitude > 23),]
bisp_coords <- bisp_coords[(bisp_coords$longitude < 81) & (bisp_coords$longitude > 65),]

# Create Clusters --------------------------------------------------------------
bisp_coords$id <- 1:nrow(bisp_coords)
bisp_coords$cluster_id <- NA

for(i in 1:nrow(bisp_coords)){
  bisp_coords_i <- bisp_coords[i,]
  within_distance <- sqrt((bisp_coords$longitude - bisp_coords_i$longitude)^2 + (bisp_coords$latitude - bisp_coords_i$latitude)^2) <= (cluster_search_radius_meters/1000/111.12)
  
  # If has a cluster ID, consider giving cluster id
  if(!is.na(bisp_coords_i$cluster_id)){
    
    # Check if all crashes in cluster are within threshold distance
    current_cluster <- bisp_coords[bisp_coords$cluster_id %in% bisp_coords_i$cluster_id,]
    within_cluster_distance <- sqrt((bisp_coords_i$longitude - current_cluster$longitude)^2 + (bisp_coords_i$latitude - current_cluster$latitude)^2) <= (max_cluster_size_meters/1000/111.12)
    
    # If a crash is not within thresh distance, give ID as cluster; otherwise, add to cluster
    if(FALSE %in% within_cluster_distance){
      bisp_coords$cluster_id[within_distance] <- bisp_coords_i$id
    } else{
      bisp_coords$cluster_id[within_distance] <- bisp_coords_i$cluster_id
    }
    
    # If doesn't have cluster ID, give ID.
  } else{
    bisp_coords$cluster_id[within_distance] <- bisp_coords_i$id
  }
  
}

bisp_cluster <- lapply(unique(bisp_coords$cluster_id), function(h){
  bisp_coords_h <- bisp_coords[bisp_coords$cluster_id %in% h,]
  
  latitude <- (max(bisp_coords_h$latitude) + min(bisp_coords_h$latitude))/2
  longitude <- (max(bisp_coords_h$longitude) + min(bisp_coords_h$longitude))/2
  
  max_dist_to_center_dd <- max(sqrt((latitude - bisp_coords_h$latitude)^2 + (longitude - bisp_coords_h$longitude)^2))
  max_dist_to_center_km <- max_dist_to_center_dd*111.12
  
  df_out <- data.frame(cluster_id = h,
                       cluster_size = nrow(bisp_coords_h), 
                       latitude,
                       longitude,
                       max_dist_to_center_dd,
                       max_dist_to_center_km)
  
  return(df_out)
}) %>% bind_rows

# Export -----------------------------------------------------------------------
write.csv(bisp_cluster, file = file.path(bisp_geocodes_file_path, "bisp_cluster_centroids.csv"), row.names=F)

bisp_coords %>%
  dplyr::select(uid, cluster_id) %>%
  write.csv(file = file.path(bisp_geocodes_file_path,  "bisp_uid_cluster_crosswalk.csv"), row.names=F)


