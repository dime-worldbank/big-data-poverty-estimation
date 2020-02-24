# Explore Facebook Ad API

# Extractions dau (daily active users) and mau (monthly active users) from locations
# using specified parameters.

# RESOURCES
# https://github.com/SofiaG1l/Using_Facebook_API

# TODO
# 1. Time Range
   # https://developers.facebook.com/docs/marketing-api/insights/
   # 'time_range={"since":"2015-03-01","until":"2015-03-31"}'
   # "'time_range':{'since':'",parameters_df_i$date_start,"',",
   # "'until':'",parameters_df_i$date_end,"'},",

library(tidyverse)
library(lubridate)
library(jsonlite)
library(httr)
library(curl)

# Load HH Coordinates ----------------------------------------------------------
bisp_coords <- read_dta(file.path(bisp_geocodes_file_path, "GPS_uid_crosswalk.dta"))

bisp_coords <- bisp_coords[!is.na(bisp_coords$GPSN),]
bisp_coords$latitude <- get_lat_lon(bisp_coords$GPSN)
bisp_coords$longitude <- get_lat_lon(bisp_coords$GPSE)
bisp_coords$uid <- bisp_coords$uid %>% as.numeric()

# Some coordinates are bad; remove those
bisp_coords <- bisp_coords[(bisp_coords$latitude < 37) & (bisp_coords$latitude > 23),]
bisp_coords <- bisp_coords[(bisp_coords$longitude < 81) & (bisp_coords$longitude > 65),]

bisp_coords_cluster <- create_clusters(bisp_coords, "latitude", "longitude", 1000, 1000)

#### Create Clusters
cluster_coords <- bisp_coords_cluster %>%
  group_by(cluster_id) %>%
  summarise(latitude = mean(latitude),
            longitude = mean(longitude))

uid_clusterid_crosswalk <- bisp_coords_cluster %>%
  dplyr::select(uid, cluster_id)

# Setup Credentials ------------------------------------------------------------
api_keys <- read.csv(file.path(webscraping_api_filepath, "api_keys.csv"), stringsAsFactors=F) %>%
  filter(Service == "facebook_marketing_ad",
         Details == "robmarty3@gmail.com")

token <- api_keys$Key[api_keys$Account %in% "token"]
creation_act <- api_keys$Key[api_keys$Account %in% "creation_act"]
version <- api_keys$Key[api_keys$Account %in% "version"]

# Parameters -------------------------------------------------------------------
# TODO: Adapt this to expand from other parameters
parameters_df <- data.frame(
  radius_km = 5,
  gender = c("1,2","1","2"),
  date_start = "2015-03-01",
  date_end = "2015-08-01",
  age_min = 13,
  age_max = 65,
  facebook_positions = "'feed','instant_article','instream_video','marketplace'",
  device_platforms = "'mobile','desktop'",
  publisher_platforms = "'facebook','messenger'",
  messenger_positions = "'messenger_home'",
  stringsAsFactors = F
)

make_query_location_i <- function(loc_i, coords_df, coords_id_name, parameters_df_i, version, creation_act, token){

  # Stall if not connected to internet
  while(!curl::has_internet()){ Sys.sleep(30); print("Looking for internet") }
  
  query_val_df <- tryCatch({

    query <- paste0("https://graph.facebook.com/",version,
                    "/act_",creation_act,
                    "/delivery_estimate?access_token=",token,
                    "&include_headers=false&method=get&pretty=0&suppress_http_code=1&method=get&optimization_goal=REACH&pretty=0&suppress_http_code=1&targeting_spec={",
                    "'geo_locations':{'custom_locations':[{'latitude':",coords_df$latitude[loc_i] %>% substring(1,7),",",
                    "'longitude':",coords_df$longitude[loc_i] %>% substring(1,7),",",
                    "'radius':",parameters_df_i$radius_km,",",
                    "'distance_unit':'kilometer'}]},",
                    "'genders':[",parameters_df_i$gender,"],",
                    "'age_min':",parameters_df_i$age_min,",",
                    "'age_max':",parameters_df_i$age_max, #",",
                    #"'facebook_positions':[",parameters_df_i$facebook_positions,"],",
                    #"'device_platforms':[",parameters_df_i$device_platforms,"],",
                    #"'publisher_platforms':[",parameters_df_i$publisher_platforms,"],",
                    #"'messenger_positions':[",parameters_df_i$messenger_positions,"]",
                    "}")
    
    query_val <- url(query) %>% fromJSON
    
    # If there is no error
    if(is.null(query_val$error) | query %in% "error"){
      query_val_df <- query_val$data
      query_val_df$daily_outcomes_curve <- NULL
      
      for(var in names(parameters_df_i)) query_val_df[[var]] <- parameters_df_i[[var]]
      query_val_df[[coords_id_name]] <- coords_df[[coords_id_name]][loc_i]
      query_val_df$api_call_time_utc <- Sys.time() %>% with_tz(tzone = "UTC")
      
      print(paste0(loc_i,": ", query_val_df$estimate_mau," ", query_val_df$estimate_dau))
      Sys.sleep(18)
      
      # If there is an error, print the error and make output null  
    } else{
      print(paste(loc_i, "-----------------------------------------------------"))
      print(query_val)
      query_val_df <- NULL
    }
    
    query_val_df
  
  },error = function(e){
    print("ERROR")
    Sys.sleep(120)
    return(NULL)
  })
  

  return(query_val_df)
}

#queries_all_df_2 <- lapply(1:nrow(cluster_coords), make_query_location_i, bisp_coords, parameters_df[2,], version, creation_act, token) %>% bind_rows()
#queries_all_df_3 <- lapply(1:nrow(cluster_coords), make_query_location_i, bisp_coords, parameters_df[3,], version, creation_act, token) %>% bind_rows()

#queries_all_df <- bind_rows(queries_all_df_1, queries_all_df_2, queries_all_df_3)

# Export -----------------------------------------------------------------------
saveRDS(uid_clusterid_crosswalk, file.path(final_data_file_path, "BISP", "Individual Datasets", "facebook_marketing_extract_clusterid_crosswalk.Rds"))

#queries_all_df_1 <- lapply(1:nrow(cluster_coords), make_query_location_i, cluster_coords, "cluster_id", parameters_df[1,], version, creation_act, token) %>% bind_rows()
#saveRDS(queries_all_df_1, file.path(final_data_file_path, "BISP", "Individual Datasets", "facebook_marketing_extract_1.Rds"))

queries_all_df_3 <- lapply(1:nrow(cluster_coords), make_query_location_i, cluster_coords, "cluster_id", parameters_df[3,], version, creation_act, token) %>% bind_rows()
saveRDS(queries_all_df_3, file.path(final_data_file_path, "BISP", "Individual Datasets", "facebook_marketing_extract_3.Rds"))

queries_all_df_2 <- lapply(1:nrow(cluster_coords), make_query_location_i, cluster_coords, "cluster_id", parameters_df[2,], version, creation_act, token) %>% bind_rows()
saveRDS(queries_all_df_2, file.path(final_data_file_path, "BISP", "Individual Datasets", "facebook_marketing_extract_2.Rds"))













