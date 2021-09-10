# Facebook Marketing API: Scrape Data

# DESCRIPTION: Scrape data from Facebook's Marketing API at survey locations. 
# Extract DAU and MAU across a variety of parameters. Code loops through survey
# locations and scrapes data across multiple parameters for that survey location.
# Because of the API rate limits, this code can take a while (weeks). Consequently,
# a data file is saved for each survey location; this allows the code to easily
# check the locations where data has already been scraped and to skip those 
# locations. Another code file appends all these files into one file.

# RATE LIMIT: 200 calls/hour
# 60*60/200

# RESOURCES
# https://developers.facebook.com/docs/marketing-api/audiences/reference/basic-targeting
# https://developers.facebook.com/docs/marketing-api/audiences/reference/advanced-targeting
# https://github.com/SofiaG1l/Using_Facebook_API

# MAIN PARAMETERS
SURVEY_NAME   <- "DHS"
API_KEY_EMAIL <- "robertandrewmarty3@gmail.com" # robmarty3@gmail.com, robertandrewmarty3@gmail.com, robertandrewmarty@gmail.com

# SCRAPING PARAMETERS
# Determine if want to skip locations where data has already been scraped. Also,
# can decide if want to scrape only locations with a uid that is even or odd,
# or scrape the UIDs in reverse. These second parameters are useful if want
# to use multiple API keys and run multiple instances of this script concurrently
# (in short, to help divide and conquer which locations are scraped).
SKIP_IF_ALREAD_SCRAPED <- T

# If only want to scrape locations where the survey uid is even, odd or all.
SCRAPE_EVEN_ODD <- "even" # "even", "odd", "all". 

# If want to scrape APIs in reverse order
change_order <- "" # "reverse", "start_middle_to_front", "start_middle_to_back"

# Load Coordinates -------------------------------------------------------------
df <- readRDS(file.path(dhs_dir, "FinalData", "Individual Datasets", "survey_socioeconomic.Rds"))

if(SURVEY_NAME %in% "DHS"){
  df <- df[df$most_recent_survey %in% T,]
}

# Setup Credentials ------------------------------------------------------------
api_keys <- read.csv(file.path(api_key_dir, "api_keys.csv"), stringsAsFactors=F) %>%
  filter(Service == "facebook_marketing_ad",
         Details == API_KEY_EMAIL)

token <- api_keys$Key[api_keys$Account %in% "token"] %>% str_squish()
creation_act <- api_keys$Key[api_keys$Account %in% "creation_act"] %>% str_replace_all("ACT_", "") %>% str_squish()
version <- api_keys$Key[api_keys$Account %in% "version"] %>% str_squish()

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
  behavior = c(
    # Network access
    "{'id':6017253486583}", # Facebook access (network type): 2G
    "{'id':6017253511583}", # Facebook access (network type): 3G
    "{'id':6017253531383}", # Facebook access (network type): 4G
    "{'id':6015235495383}", # Facebook access (network type): WiFi
    
    # Mobile OS
    "{'id':6004386044572}", # Facebook access (mobile): Android devices
    "{'id':6004384041172}", # Facebook access (mobile): Apple (iOS) devices
    "{'id':6004385895772}", # Facebook access (mobile): Windows phones
    
    # High end phones
    "{'id':6092512462983}", # Facebook access (mobile): iPhone X
    "{'id':6092512462983},{'id':6092512412783},{'id':6092512424583}", # iPhone X/8/8 Plus
    "{'id':6106224431383}", # Owns: Galaxy S9+
    "{'id':6075237200983},{'id':6075237226583},{'id':6106223987983},{'id':6106224431383}", #Samsung Galaxy phone S8/S8+/S9/S9+ 
    "{'id':6075237200983},{'id':6075237226583},{'id':6106223987983},{'id':6106224431383},{'id':6092512462983},{'id':6092512412783},{'id':6092512424583}", # iPhone X/8/8 Plus or Samsung Galaxy phone S8/S8+/S9/S9+ 
    
    # Other device types
    "{'id':6004382299972}", # Facebook access (mobile): all mobile devices
    "{'id':6004383149972}", # Facebook access (mobile): feature phones
    "{'id':6004383049972}", # Facebook access (mobile): smartphones and tablets
    "{'id':6016286626383}", # Facebook access (mobile): tablets
    "{'id':6023460590583}", # Owns: Cherry Mobile
    "{'id':6056265212183}", # Owns: VIVO devices
    "{'id':6011390261383}", # Owns: Huawei
    "{'id':6056265200983}", # Owns: Oppo
    "{'id':6056265200983},{'id':6056265212183},{'id':6023460590583}", # Oppo/Vivo/Cherry
    "{'id':6004386010572}", # Facebook access (mobile): Samsung Android mobile devices
    
    # OTHER
    "{'id':6002714895372}", # Frequent Travelers
    "{'id':6022788483583}", # Frequent international travelers
    "{'id':6002714898572}", # Small business owners
    "{'id':6003808923172}", # Technology early adopters
    "{'id':6004854404172}" # Facebook access: older devices and OS
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
               #"{'id':6004115167424}", # Physical exercise
               "{'id':6003384248805}", # Fitness and wellness
               "{'id':6007828099136}" # Luxury Goods
  ), 
  stringsAsFactors = F
)

# Append
parameters_df <- bind_rows(
  parameters_df_all,
  parameters_df_educ,
  parameters_df_behaviors,
  parameters_df_interests
)

parameters_df$param_id <- 1:nrow(parameters_df)

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
  
  parameters_df_i <- param_df[param_df$param_id %in% param_i,]
  
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
      print(paste0(param_i,": ", coords_df$uid, " ", query_val_df$estimate_mau," ", query_val_df$estimate_dau, " ", API_KEY_EMAIL))
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
# Determine time to pause when scraping the API due to API rate limits.

#### Pause after scrape each parameter
sleep_time_after_param <- 0.5

#### Pause after each location
number_locs_per_hour <- ceiling(200/nrow(parameters_df))

seconds_in_hour <- 60*60
sleep_time_after_loc <- (seconds_in_hour/number_locs_per_hour)
sleep_time_after_loc <- sleep_time_after_loc - nrow(parameters_df)*sleep_time_after_param
sleep_time_after_loc <- sleep_time_after_loc - 100

# Implement Function and Export ------------------------------------------------
## Grab country codes
country_code_all <- df$country_code %>% unique()
country_code_all <- country_code_all[!(country_code_all %in% c("ID"))]

## Restrict dataset to script by whether uid is even or odd
odd <- df$uid %>% str_sub(-2,-1) %>% as.numeric() %>% `%%`(2)

if(SCRAPE_EVEN_ODD %in% "odd"){
  df <- df[odd %in% 1,]
}

if(SCRAPE_EVEN_ODD %in% "even"){
  df <- df[odd %in% 0,]
}

## UIDs to scrape
for(country_code_i in c("NG", "GA", country_code_all)){
  
  df_c <- df[df$country_code %in% country_code_i,]
  
  country_uids <- unique(df_c$uid) %>% as.character()
  
  if(change_order == "reverse") country_uids <- rev(country_uids)
  
  if(change_order == "start_middle_to_front"){
    mid_point <- floor(length(country_uids)/2)
    country_uids <- c(rev(country_uids[1:mid_point]),
                      rev(country_uids[(mid_point+1):length(country_uids)]))
  }
  
  if(change_order == "start_middle_to_back"){
    mid_point <- floor(length(country_uids)/2)
    country_uids <- c(rev(country_uids[(mid_point+1):length(country_uids)]),
                      rev(country_uids[1:mid_point]))
  }
  
  for(uid_i in country_uids){
    
    df_i <- df_c[df_c$uid %in% uid_i,]
    
    if(df_i$urban_rural %in% "U") RADIUS_i <- 5 #km
    if(df_i$urban_rural %in% "R") RADIUS_i <- 10 #km
    
    OUT_PATH <- file.path(dropbox_dir, "Data", SURVEY_NAME,  "FinalData", "Individual Datasets",
                          "fb_mau_individual_datasets", paste0("fb_",uid_i,"_radius",RADIUS_KM,"km.Rds"))
    
    if(SKIP_IF_ALREAD_SCRAPED & file.exists(OUT_PATH)){
      print(paste("Skip", uid_i))
    } else{
      
      parameters_df$radius_km <- RADIUS_i
      
      df_out <- map_df(parameters_df$param_id, make_query_location_i, 
                       parameters_df,
                       df_i,
                       version,
                       creation_act,
                       token,
                       sleep_time = sleep_time_after_param)
      
      if(nrow(df_out) == nrow(parameters_df)) saveRDS(df_out, OUT_PATH)
      rm(df_out)
      
      Sys.sleep(sleep_time_after_loc)
    }
    
  }
}

