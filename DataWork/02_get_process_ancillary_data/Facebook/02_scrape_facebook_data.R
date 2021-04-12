# Explore Facebook Ad API

# Extractions dau (daily active users) and mau (monthly active users) from locations
# using specified parameters.

# RATE LIMIT: 200 calls/hour
# 60*60/200

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

# Load Coordinates -------------------------------------------------------------
coords_df <- readRDS(file.path(project_file_path, "Data", "Facebook", "FinalData", 
                               "locations_to_scrape", "cluster_locations.Rds"))

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

#loc_i = 1
#parameters_df_i = parameters_df[1,]
make_query_location_i <- function(loc_i, 
                                  coords_df,
                                  parameters_df_i, 
                                  version, 
                                  creation_act, 
                                  token){
  
  # Query Facebook Marketing API
  # ARGs:
  # --loc_i: Numeric id of which row to use from `coords_df`
  # --coords_df: Dataframe with latitude and longitude variables
  # --parameters_df_i: Dataframe with parameters
  # --version: Facebook marketing API verion
  # --creation_act: Creation act (associated with API key/account)
  # --token: API token/key
  
  # Stall if not connected to internet
  while(!curl::has_internet()){ Sys.sleep(30); print("Looking for internet") }
  
  # Make query and prep dataframe with results and parameter
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
    
    #### If there is no error
    if(is.null(query_val$error)){
      
      ## Marketing info to dataframe
      query_val_df <- query_val$data
      query_val_df$daily_outcomes_curve <- NULL
      
      ## Add parameter info
      for(var in names(parameters_df_i)) query_val_df[[var]] <- parameters_df_i[[var]]
      
      ## Add cluster info
      query_val_df$cluster_id <- coords_df$cluster_id[loc_i]
      query_val_df$latitude   <- coords_df$latitude[loc_i]
      query_val_df$longitude  <- coords_df$longitude[loc_i]
      
      ## Add time
      query_val_df$api_call_time_utc <- Sys.time() %>% with_tz(tzone = "UTC")
      
      ## Print result and sleep (sleep needed b/c of rate limiting)
      print(paste0(loc_i,": ", query_val_df$estimate_mau," ", query_val_df$estimate_dau))
      Sys.sleep(20) # really just need 18; 20 just in case
      
      #### If there is an error, print the error and make output null  
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

# Implement Function and Export ------------------------------------------------

queries_all_df_1 <- map_df(1:3, make_query_location_i, coords_df, parameters_df[1,], version, creation_act, token)

time <- Sys.time() %>% as.character() %>% str_replace_all("[[:punct:]]| ", "")
saveRDS(queries_all_df_1,
        file.path(project_file_path, "Data", "Facebook", "FinalData", 
                  "mau_dau_results", 
                  paste0("facebook_marketing_",time,".Rds")))
write.csv(queries_all_df_1,
          file.path(project_file_path, "Data", "Facebook", "FinalData", 
                    "mau_dau_results", 
                    paste0("facebook_marketing_",time,".csv")),
          row.names = F)

