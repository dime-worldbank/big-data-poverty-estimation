# Explore Facebook Ad API

# Extractions dau (daily active users) and mau (monthly active users) from locations
# using specified parameters.

# RATE LIMIT: 200 calls/hour
# 60*60/200

# PARAMETERS
# https://developers.facebook.com/docs/marketing-api/audiences/reference/basic-targeting
# https://developers.facebook.com/docs/marketing-api/audiences/reference/advanced-targeting

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
library(haven)

SKIP_IF_ALREAD_SCRAPED <- F

# Load Coordinates -------------------------------------------------------------
df <- readRDS(file.path(secure_file_path, "Data", SURVEY_NAME,  "FinalData - PII", "GPS_uid_crosswalk.Rds"))

if(SURVEY_NAME %in% "DHS"){
  df <- df[df$uid %>% str_detect("PK2017"),]
}

# Setup Credentials ------------------------------------------------------------
api_keys <- read.csv(file.path(webscraping_api_filepath, "api_keys.csv"), stringsAsFactors=F) %>%
  filter(Service == "facebook_marketing_ad",
         Details == "robmarty3@gmail.com")

token <- api_keys$Key[api_keys$Account %in% "token"]
creation_act <- api_keys$Key[api_keys$Account %in% "creation_act"]
version <- api_keys$Key[api_keys$Account %in% "version"]

# Parameters -------------------------------------------------------------------
AGE_MIN = 18
AGE_MAX = 65
FACEBOOK_POSITIONS = "'feed','instant_article','instream_video','marketplace'"
RADIUS_KM = 10

# All
parameters_df_all <- data.frame(
  radius_km = RADIUS_KM,
  gender = c("1,2"),
  age_min = AGE_MIN,
  age_max = AGE_MAX,
  facebook_positions = FACEBOOK_POSITIONS,
  device_platforms = "'mobile','desktop'",
  publisher_platforms = "'facebook','messenger'",
  messenger_positions = "'messenger_home'",
  stringsAsFactors = F
)

# By education status
parameters_df_educ <- data.frame(
  radius_km = RADIUS_KM,
  gender = c("1,2"),
  age_min = AGE_MIN,
  age_max = AGE_MAX,
  facebook_positions = FACEBOOK_POSITIONS,
  device_platforms = "'mobile','desktop'",
  publisher_platforms = "'facebook','messenger'",
  messenger_positions = "'messenger_home'",
  education_statuses = c("1,4,13",
                         "2,5,6,7,8,9,10,11"),
  stringsAsFactors = F
)

# By device type
parameters_df_device <- data.frame(
  radius_km = RADIUS_KM,
  gender = c("1,2"),
  age_min = AGE_MIN,
  age_max = AGE_MAX,
  facebook_positions = FACEBOOK_POSITIONS,
  device_platforms = "'mobile','desktop'",
  publisher_platforms = "'facebook','messenger'",
  messenger_positions = "'messenger_home'",
  user_os = c("'iOS'",
              "'Android'",
              #"'iOS_ver_2.0_to_3.0'",
              #"'iOS_ver_4.0_to_5.0'",
              #"'iOS_ver_6.0_to_7.0'",
              #"'iOS_ver_8.0_and_above'",
              "'iOS_ver_9.0_and_above'",
              #"'Android_ver_2.0_to_3.2'",
              #"'Android_ver_4.0_to_5.1'",
              #"'Android_ver_6.0_to_7.1'",
              #"'Android_ver_7.0_and_above'",
              "'Android_ver_8.0_and_above'"),
  stringsAsFactors = F
)

# Wireless carrier
parameters_df_carrier <- data.frame(
  radius_km = RADIUS_KM,
  gender = c("1,2"),
  age_min = AGE_MIN,
  age_max = AGE_MAX,
  facebook_positions = FACEBOOK_POSITIONS,
  device_platforms = "'mobile','desktop'",
  publisher_platforms = "'facebook','messenger'",
  messenger_positions = "'messenger_home'",
  wireless_carrier = "'wifi'",
  stringsAsFactors = F
)

# Behaviors
parameters_df_behaviors <- data.frame(
  radius_km = RADIUS_KM,
  gender = c("1,2"),
  age_min = AGE_MIN,
  age_max = AGE_MAX,
  facebook_positions = FACEBOOK_POSITIONS,
  device_platforms = "'mobile','desktop'",
  publisher_platforms = "'facebook','messenger'",
  messenger_positions = "'messenger_home'",
  behavior = c("{'id':6002714895372}", # Frequent Travelers
               "{'id':6022788483583}", # Frequent international travelers
               "{'id':6002714898572}", # Small business owners
               "{'id':6003808923172}", # Technology early adopters
               "{'id':6004386044572}", # Facebook access (mobile): Android devices
               "{'id':6004384041172}", # Facebook access (mobile): Apple (iOS) devices
               "{'id':6004854404172}", # Facebook access: older devices and OS
               "{'id':6007078565383}", # New smartphone and tablet users
               "{'id':6017253486583}", # Facebook access (network type): 2G
               "{'id':6017253511583}", # Facebook access (network type): 3G
               "{'id':6017253531383}", # Facebook access (network type): 4G
               "{'id':6015235495383}" # Facebook access (network type): WiFi
  ), 
  stringsAsFactors = F
)

# Interests
parameters_df_interests <- data.frame(
  radius_km = RADIUS_KM,
  gender = c("1,2"),
  age_min = AGE_MIN,
  age_max = AGE_MAX,
  facebook_positions = FACEBOOK_POSITIONS,
  device_platforms = "'mobile','desktop'",
  publisher_platforms = "'facebook','messenger'",
  messenger_positions = "'messenger_home'",
  interest = c("{'id':6003012317397}", # Gambling
               "{'id':6004115167424}" # Physical exercise
  ), 
  stringsAsFactors = F
)

# Append
parameters_df <- bind_rows(
  parameters_df_all,
  parameters_df_educ,
  parameters_df_device,
  #parameters_df_carrier,
  parameters_df_behaviors,
  parameters_df_interests
)

param_df <- parameters_df

# Function to extract data -----------------------------------------------------
make_query_location_i <- function(param_i,
                                  param_df,
                                  coords_df,
                                  version, 
                                  creation_act, 
                                  token,
                                  sleep_time = 20){
  
  # Query Facebook Marketing API
  # ARGs:
  # --loc_i: Numeric id of which row to use from `coords_df`
  # --coords_df: Dataframe with latitude and longitude variables
  # --parameters_df_i: Dataframe with parameters
  # --version: Facebook marketing API verion
  # --creation_act: Creation act (associated with API key/account)
  # --token: API token/key
  
  parameters_df_i <- param_df[param_i,]
  
  # Stall if not connected to internet
  while(!curl::has_internet()){ Sys.sleep(30); print("Looking for internet") }
  
  # Make query and prep dataframe with results and parameter
  query_val_df <- tryCatch({
    
    query <- paste0("https://graph.facebook.com/",version,
                    "/act_",creation_act,
                    "/delivery_estimate?access_token=",token,
                    "&include_headers=false&method=get&pretty=0&suppress_http_code=1&method=get&optimization_goal=REACH&pretty=0&suppress_http_code=1&targeting_spec={",
                    "'geo_locations':{'location_types':['home'],'custom_locations':[{'latitude':",coords_df$latitude %>% substring(1,7),",",
                    "'longitude':",coords_df$longitude %>% substring(1,7),",",
                    "'radius':",parameters_df_i$radius_km,",",
                    "'distance_unit':'kilometer'}]},",
                    ifelse(is.na(parameters_df_i$education_statuses), "", 
                           paste0("'education_statuses':[", parameters_df_i$education_statuses, "],")), 
                    ifelse(is.na(parameters_df_i$user_os), "", 
                           paste0("'user_os':[", parameters_df_i$user_os, "],")), 
                    ifelse(is.na(parameters_df_i$wireless_carrier), "", 
                           paste0("'wireless_carrier':[", parameters_df_i$wireless_carrier, "],")), 
                    ifelse(is.na(parameters_df_i$behavior), "", 
                           paste0("'behaviors':[", parameters_df_i$behavior, "],")), 
                    ifelse(is.na(parameters_df_i$interest), "", 
                           paste0("'interests':[", parameters_df_i$interest, "],")), 
                    "'genders':[",parameters_df_i$gender,"],",
                    "'age_min':",parameters_df_i$age_min,",",
                    "'age_max':",parameters_df_i$age_max, #",",
                    #"'facebook_positions':[",parameters_df_i$facebook_positions,"],",
                    #"'device_platforms':[",parameters_df_i$device_platforms,"],",
                    #"'publisher_platforms':[",parameters_df_i$publisher_platforms,"],",
                    #"'messenger_positions':[",parameters_df_i$messenger_positions,"]",
                    "}")
    
    query_val <- url(query) %>% fromJSON
    
    #### If there is no error
    if(is.null(query_val$error)){
      
      ## Marketing info to dataframe
      query_val_df <- query_val$data
      query_val_df$daily_outcomes_curve <- NULL
      
      ## Add parameter info
      for(var in names(parameters_df_i)) query_val_df[[var]] <- parameters_df_i[[var]]
      
      ## Add cluster info
      query_val_df$param_id   <- param_i
      query_val_df$uid <- coords_df$uid
      query_val_df$latitude   <- coords_df$latitude
      query_val_df$longitude  <- coords_df$longitude
      
      ## Add time
      query_val_df$api_call_time_utc <- Sys.time() %>% with_tz(tzone = "UTC")
      
      ## Print result and sleep (sleep needed b/c of rate limiting)
      print(paste0(param_i,": ", query_val_df$estimate_mau," ", query_val_df$estimate_dau))
      Sys.sleep(sleep_time) # really just need 18; 20 just in case
      
      #### If there is an error, print the error and make output null  
    } else{
      print(paste(param_i, "-----------------------------------------------------"))
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

# Determine Sleep Time ---------------------------------------------------------
# 200 calls per hour
sleep_time_after_param <- 0.5

number_locs_per_hour <- ceiling(200/nrow(parameters_df))

seconds_in_hour <- 60*60
sleep_time_after_loc <- (seconds_in_hour/number_locs_per_hour)
sleep_time_after_loc <- sleep_time_after_loc - nrow(parameters_df)*sleep_time_after_param
sleep_time_after_loc <- sleep_time_after_loc + 1

# Implement Function and Export ------------------------------------------------
for(uid_i in unique(df$uid)){
  
  OUT_PATH <- file.path(project_file_path, "Data", SURVEY_NAME,  "FinalData", "Individual Datasets",
                        "fb_mau_individual_datasets", paste0("fb_",uid_i,".Rds"))
  
  if(SKIP_IF_ALREAD_SCRAPED & file.exists(OUT_PATH)){
    print(paste("Skip", uid_i))
  } else{

    df_i <- df[df$uid %in% uid_i,]
    
    # 1:nrow(parameters_df)
    df_out <- map_df(1:nrow(parameters_df), make_query_location_i, 
                     parameters_df,
                     df_i,
                     version,
                     creation_act,
                     token,
                     sleep_time = sleep_time_after_param)
    
    saveRDS(df_out, OUT_PATH)
    rm(df_out)
    
    Sys.sleep(sleep_time_after_loc)
  }
  
}

