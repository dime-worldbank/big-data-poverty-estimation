
near_crash_thresh_meters <- 500
near_anycrash_in_cluster_thresh_meters <- 1000
clusters_near_join_meters <- .000001

# Load Data --------------------------------------------------------------------
hh_coords <- read_dta(file.path("~/Desktop", "GPS_uid_crosswalk.dta"))
hh_coords$hh_id <- 1:nrow(hh_coords)
hh_coords <- hh_coords[!is.na(hh_coords$GPSN),]

get_lat_long <- function(number) {
  deg <- floor(number / 100)
  min <- floor(number - (100 * deg))
  sec <- 100 * (number - (100 * deg) - min)
  degree <- deg + (min / 60) + (sec / 3600)
  
  return(degree)
}
hh_coords <- hh_coords %>%
  mutate(latitude = get_lat_long(GPSN),
         longitude = get_lat_long(GPSE))

# Cluster ----------------------------------------------------------------------
hh_coords$id <- 1:nrow(hh_coords)
hh_coords$cluster_id <- NA

for(i in 1:nrow(hh_coords)){
  hh_coords_i <- hh_coords[i,]
  within_distance <- sqrt((hh_coords_i$longitude - hh_coords$longitude)^2 + (hh_coords_i$latitude - hh_coords$latitude)^2) <= (near_crash_thresh_meters/1000/111.12)
  
  # If has a cluster ID, consider giving cluster id
  if(!is.na(hh_coords_i$cluster_id)){
    
    # Check if all crashes in cluster are within threshold distance
    current_cluster <- hh_coords[hh_coords$cluster_id %in% hh_coords_i$cluster_id,]
    within_cluster_distance <- sqrt((hh_coords_i$longitude - current_cluster$longitude)^2 + (hh_coords_i$latitude - current_cluster$latitude)^2) <= (near_anycrash_in_cluster_thresh_meters/1000/111.12)
    
    # If a crash is not within thresh distance, give ID as cluster; otherwise, add to cluster
    if(FALSE %in% within_cluster_distance){
      hh_coords$cluster_id[within_distance] <- hh_coords_i$id
    } else{
      hh_coords$cluster_id[within_distance] <- hh_coords_i$cluster_id
    }
    
    # If doesn't have cluster ID, give ID.
  } else{
    hh_coords$cluster_id[within_distance] <- hh_coords_i$id
  }
  
}

hh_coords$cluster_id %>% unique %>% length

# Create Cluster Centroids -----------------------------------------------------
hh_clusters <- lapply(unique(hh_coords$cluster_id), function(cluster_id){
  hh_coords_ci <- hh_coords[hh_coords$cluster_id == cluster_id,]
  
  coordinates(hh_coords_ci) <- ~longitude+latitude
  out <- gCentroid(hh_coords_ci) %>% coordinates %>%
    as.data.frame %>%
    dplyr::mutate(cluster_id = cluster_id,
                  dist_max = max(gDistance(hh_coords_ci, byid=T))) 
  
  return(out)
}) %>% bind_rows %>%
  dplyr::rename(longitude=x,
                latitude=y)

# Export -----------------------------------------------------------------------
hh_coords <- hh_coords %>%
  dplyr::select(uid, hh_id, cluster_id)
write.csv(hh_coords, file.path(rawdata_file_path, "Landsat", "bisp_households", "bisp_hh_cluster_id_crosswalk.csv"), row.names = F)
write.csv(hh_clusters, file.path(rawdata_file_path, "Landsat", "bisp_households", "bisp_cluster_coords.csv"), row.names = F)

max(hh_clusters$dist_max * 111.12)
nrow(hh_clusters)




