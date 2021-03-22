# Useful functions for project

get_lat_lon <- function(number){
  # Transform lat/lon degree/minute/second data from OPM to decimal degrees.
  
  deg = floor(number / 100)
  min = floor(number - (100 * deg))
  sec = 100 * (number - (100 * deg) - min)
  degree = deg + (min / 60) + (sec / 3600)
  
  return(degree)
}




# Function to CLuster Unique Crashes into Crash 

create_clusters <- function(crashes_df,
                                  lat_var,
                                  lon_var,
                                  near_crash_thresh_meters,
                                  near_anycrash_in_cluster_thresh_meters){
  
  # DESCRIPTION:
  # Cluster unique crashes into crash clusters. Grab all crashes within 
  # [near_crash_thresh_meters] meters, not allowing the maximum distance of the
  # cluster to grow beyond [near_anycrash_in_cluster_thresh_meters]. Currently
  # assumes WGS84
  
  # PARAMETERS
  # crashes_df: Crash dataframe
  # lat_var: Name of variable indicating latitude
  # lon_var: Name of variable indicating longitude
  # near_crash_thresh_meters: Grab all crashes within this distance.
  # near_anycrash_in_cluster_thresh_meters: Don't allow distance between any
  # points in crash cluster to go beyond this.
  
  crashes_df$longitude <- crashes_df[[lon_var]]
  crashes_df$latitude <- crashes_df[[lat_var]]
  
  # Sory by latitude and longitude
  crashes_df <- crashes_df[order(crashes_df$latitude, crashes_df$longitude),]
  
  # Give cluster ID --------------------------------------------------------------
  crashes_df$id <- 1:nrow(crashes_df)
  crashes_df$cluster_id <- NA
  
  for(i in 1:nrow(crashes_df)){
    if((i %% 500) %in% 0) print(i)
    
    crashes_df_i <- crashes_df[i,]
    within_distance <- sqrt((crashes_df_i$longitude - crashes_df$longitude)^2 + (crashes_df_i$latitude - crashes_df$latitude)^2) <= (near_crash_thresh_meters/1000/111.12)
    
    # If has a cluster ID, consider giving cluster id
    if(!is.na(crashes_df_i$cluster_id)){
      
      # Check if all crashes in cluster are within threshold distance
      current_cluster <- crashes_df[crashes_df$cluster_id %in% crashes_df_i$cluster_id,]
      within_cluster_distance <- sqrt((crashes_df_i$longitude - current_cluster$longitude)^2 + (crashes_df_i$latitude - current_cluster$latitude)^2) <= (near_anycrash_in_cluster_thresh_meters/1000/111.12)
      
      # If a crash is not within thresh distance, give ID as cluster; otherwise, add to cluster
      if(FALSE %in% within_cluster_distance){
        crashes_df$cluster_id[within_distance] <- crashes_df_i$id
      } else{
        crashes_df$cluster_id[within_distance] <- crashes_df_i$cluster_id
      }
      
      # If doesn't have cluster ID, give ID.
    } else{
      crashes_df$cluster_id[within_distance] <- crashes_df_i$id
    }
    
  }
  
  # Create Convex Hull -----------------------------------------------------------
  # Convert to Spatial Points Object
  #coordinates(crashes_df) <- ~longitude+latitude
  #crs(crashes_df) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
  
  # Raster aggregate
  #crashes_df <- gBuffer(crashes_df, byid=T, width=0.000001)
  #crashes_df$N_crashes <- 1
  #crashes_cluster <- raster::aggregate(crashes_df, by="cluster_id", 
  #                                     sums=list(list(sum, 'N_crashes')))
  
  #crashes_cluster_hull <- gConvexHull(crashes_cluster, byid = T)
  #crashes_cluster_hull$id <- 1:length(crashes_cluster_hull)
  #crashes_cluster_hull$N_crashes <- crashes_cluster$N_crashes
  
  return(crashes_df)
}







