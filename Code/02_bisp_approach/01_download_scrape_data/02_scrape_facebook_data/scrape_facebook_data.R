# Explore Facebook Ad API

# Extractions dau (daily active users) and mau (monthly active users) from locations
# using specified parameters.

# RESOURCES
# https://github.com/SofiaG1l/Using_Facebook_API

library(tidyverse)
library(lubridate)
library(jsonlite)
library(httr)

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

nrow(cluster_coords)

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
  age_min = 13,
  age_max = 65,
  facebook_positions = "'feed','instant_article','instream_video','marketplace'",
  device_platforms = "'mobile','desktop'",
  publisher_platforms = "'facebook','messenger'",
  messenger_positions = "'messenger_home'",
  stringsAsFactors = F
)

make_query_location_i <- function(loc_i, coords_df, coords_id_name, parameters_df_i, version, creation_act, token){

  query <- paste0("https://graph.facebook.com/",version,
                  "/act_",creation_act,
                  "/delivery_estimate?access_token=",token,
                  "&include_headers=false&method=get&pretty=0&suppress_http_code=1&method=get&optimization_goal=REACH&pretty=0&suppress_http_code=1&targeting_spec={",
                  "'geo_locations':{'custom_locations':[{'latitude':",coords_df$latitude[loc_i],",",
                  "'longitude':",coords_df$longitude[loc_i],",",
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
  query_val_df <- query_val$data
  query_val_df$daily_outcomes_curve <- NULL
  
  for(var in names(parameters_df_i)) query_val_df[[var]] <- parameters_df_i[[var]]
  query_val_df[[coords_id_name]] <- coords_df[[coords_id_name]][loc_i]
  query_val_df$api_call_time_utc <- Sys.time() %>% with_tz(tzone = "UTC")
  
  print(paste0(loc_i,": ", query_val_df$estimate_mau," ", query_val_df$estimate_dau))
  Sys.sleep(20)
  
  return(query_val_df)
}

queries_all_df_1 <- lapply(1:nrow(cluster_coords), make_query_location_i, cluster_coords, "cluster_id", parameters_df[1,], version, creation_act, token) %>% bind_rows()
#queries_all_df_2 <- lapply(1:nrow(bisp_coords), make_query_location_i, bisp_coords, parameters_df[2,], version, creation_act, token) %>% bind_rows()
#queries_all_df_3 <- lapply(1:nrow(bisp_coords), make_query_location_i, bisp_coords, parameters_df[3,], version, creation_act, token) %>% bind_rows()

#queries_all_df <- bind_rows(queries_all_df_1, queries_all_df_2, queries_all_df_3)

# Export -----------------------------------------------------------------------
saveRDS(queries_all_df_1, file.path(final_data_file_path, "BISP", "Individual Datasets", "facebook_marketing_extract.Rds"))
saveRDS(uid_clusterid_crosswalk, file.path(final_data_file_path, "BISP", "Individual Datasets", "facebook_marketing_extract_clusterid_crosswalk.Rds"))











