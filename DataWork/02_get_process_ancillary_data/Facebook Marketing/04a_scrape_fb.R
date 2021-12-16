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
# https://worldbank.github.io/connectivity_mapping/intro.html

# Load Coordinates -------------------------------------------------------------
df <- readRDS(file.path(data_dir, SURVEY_NAME, "FinalData", "Individual Datasets", "survey_socioeconomic.Rds"))

if(SURVEY_NAME %in% "DHS"){
  df <- df[df$most_recent_survey %in% T,]
}

if(SURVEY_NAME %in% "OPM"){
  df <- df %>%
    distinct(uid, .keep_all = T)
}

# Setup Credentials ------------------------------------------------------------
api_keys <- read.csv(file.path(api_key_dir, "api_keys.csv"), stringsAsFactors=F) %>%
  filter(Service == "facebook_marketing_ad")

api_keys <- api_keys[api_keys$Details != "pakistanprojectroute4@gmail.com",]

KEYS_ACCOUNTS <- api_keys$Details %>% unique()
N_KEYS <- length(KEYS_ACCOUNTS)

# Load Parameters to Scrape ----------------------------------------------------
parameters_df <- readRDS(file.path(data_dir, "Facebook Marketing", "FinalData", "facebook_marketing_parameters.Rds"))

# Determine Sleep Time ---------------------------------------------------------
# Determine time to pause when scraping the API due to API rate limits.

#### Pause after scrape each parameter
sleep_time_after_param <- 0

#### Pause after each location
number_locs_per_hour <- ceiling(200/nrow(parameters_df))

seconds_in_hour <- 60*60
sleep_time_after_loc <- (seconds_in_hour/number_locs_per_hour)
sleep_time_after_loc <- sleep_time_after_loc - nrow(parameters_df)*sleep_time_after_param
sleep_time_after_loc <- sleep_time_after_loc - 105

sleep_time_after_loc <- sleep_time_after_loc / N_KEYS

sleep_time_after_loc <- sleep_time_after_loc - 14

# Implement Function and Export ------------------------------------------------
## Grab country codes
country_code_all <- df$country_code %>% unique()

# Repeat in case missed some due to error, so will go back and check
country_code_all_rep <- c(country_code_all,
                          country_code_all,
                          country_code_all,
                          country_code_all,
                          country_code_all) %>% 
  sort()

country_code_all_rep <- c(country_code_all) %>% 
  sort()

## UIDs to scrape
KEY_i  <- 1

#### Loop over countries
for(country_code_i in country_code_all_rep){
  print(country_code_i)
  
  df_c <- df[df$country_code %in% country_code_i,]
  
  country_uids <- unique(df_c$uid) %>% as.character()
  
  #### Loop over UIDs within countries
  for(uid_i in country_uids){
    
    df_i <- df_c[df_c$uid %in% uid_i,]
    
    OUT_PATH <- file.path(dropbox_dir, "Data", SURVEY_NAME,  "FinalData", "Individual Datasets",
                          "fb_mau_individual_datasets", paste0("fb_",uid_i,".Rds"))
    
    if(REPLACE_IF_EXTRACTED | !file.exists(OUT_PATH)){
      
      ## Set keys
      print(KEY_i)
      account_i <- KEYS_ACCOUNTS[KEY_i]
      api_keys_i <- api_keys[api_keys$Details %in% account_i,]
      
      token <- api_keys_i$Key[api_keys_i$Account %in% "token"] %>% str_squish()
      creation_act <- api_keys_i$Key[api_keys_i$Account %in% "creation_act"] %>% str_replace_all("ACT_", "") %>% str_squish()
      version <- api_keys_i$Key[api_keys_i$Account %in% "version"] %>% str_squish()
      
      ## Scape
      print(account_i)
      
      # Determine Radius -------------------------------------------------------
      # ERROR will be returned when incorrect location specified
      # fb_radius_Xkm$ERROR
      
      parameters_df_i <- parameters_df[1,]
      radius <- NA
      
      #### Check 2km
      fb_radius_Xkm <- query_fb_marketing_api(location_type = "coordinates",
                                              latitude = df_i$latitude,
                                              longitude = df_i$longitude,
                                              radius = 2,
                                              radius_unit = "kilometer",
                                              education_statuses = parameters_df_i$education_statuses,
                                              behavior = parameters_df_i$behavior %>% str_split(",") %>% unlist(),
                                              interest = parameters_df_i$interest,
                                              gender = parameters_df_i$gender %>% str_split(",") %>% unlist(),
                                              age_min = parameters_df_i$age_min,
                                              age_max = parameters_df_i$age_max,
                                              version = version,
                                              creation_act = creation_act,
                                              token = token,
                                              sleep_time = 0.2)
      
      if(is.null(fb_radius_Xkm)){
        print("Too many API calls when checking radius; sleep and skip")
        KEY_i <- KEY_i + 1
        if(KEY_i > N_KEYS) KEY_i <- 1
        Sys.sleep(10)
        next
      }
      
      if(!is.null(fb_radius_Xkm$estimate_mau_upper_bound)){
        
        mau_to_check <- mean(fb_radius_Xkm$estimate_mau_upper_bound, 
                             fb_radius_Xkm$estimate_mau_lower_bound)
        
        if(mau_to_check >= 2000){
          radius <- 2
        }
      }
      
      #### Check 5km
      if(is.na(radius)){
        fb_radius_Xkm <- query_fb_marketing_api(location_type = "coordinates",
                                                latitude = df_i$latitude,
                                                longitude = df_i$longitude,
                                                radius = 5,
                                                radius_unit = "kilometer",
                                                education_statuses = parameters_df_i$education_statuses,
                                                behavior = parameters_df_i$behavior %>% str_split(",") %>% unlist(),
                                                interest = parameters_df_i$interest,
                                                gender = parameters_df_i$gender %>% str_split(",") %>% unlist(),
                                                age_min = parameters_df_i$age_min,
                                                age_max = parameters_df_i$age_max,
                                                version = version,
                                                creation_act = creation_act,
                                                token = token,
                                                sleep_time = 0.2)
        
        if(is.null(fb_radius_Xkm)){
          print("Too many API calls when checking radius; sleep and skip")
          KEY_i <- KEY_i + 1
          if(KEY_i > N_KEYS) KEY_i <- 1
          
          Sys.sleep(10)
          next
        }
        
        if(!is.null(fb_radius_Xkm$estimate_mau_upper_bound)){
          
          mau_to_check <- mean(fb_radius_Xkm$estimate_mau_upper_bound, 
                               fb_radius_Xkm$estimate_mau_lower_bound)
          
          if(mau_to_check >= 2000){
            radius <- 5
          }
        }
        
      }
      
      #### Check 10km
      if(is.na(radius)){
        # Still grab to determine whether need to scrape
        fb_radius_Xkm <- query_fb_marketing_api(location_type = "coordinates",
                                                latitude = df_i$latitude,
                                                longitude = df_i$longitude,
                                                radius = 10,
                                                radius_unit = "kilometer",
                                                education_statuses = parameters_df_i$education_statuses,
                                                behavior = parameters_df_i$behavior %>% str_split(",") %>% unlist(),
                                                interest = parameters_df_i$interest,
                                                gender = parameters_df_i$gender %>% str_split(",") %>% unlist(),
                                                age_min = parameters_df_i$age_min,
                                                age_max = parameters_df_i$age_max,
                                                version = version,
                                                creation_act = creation_act,
                                                token = token,
                                                sleep_time = 0.2)
        
        if(is.null(fb_radius_Xkm)){
          print("Too many API calls when checking radius; sleep and skip")
          KEY_i <- KEY_i + 1
          if(KEY_i > N_KEYS) KEY_i <- 1
          
          Sys.sleep(10)
          next
        }
        
        radius <- 10
      }
      
      # Determine if need to scrape --------------------------------------------
      SCRAPE_ALL_DATA <- TRUE
      
      ## Check 1: estimate_mau is 1000
      if(!is.null(fb_radius_Xkm)){
        if(!is.null(fb_radius_Xkm$estimate_mau_upper_bound)){
          if(fb_radius_Xkm$estimate_mau_upper_bound == 1000){
            
            print("estimate_1_mau = 1000; assigning 1000 to all mau")
            print(fb_radius_Xkm$estimate_mau_upper_bound)
            
            fb_df <- data.frame(uid = rep(df_i$uid, nrow(parameters_df)))
            fb_df$estimate_mau <- 1000
            fb_df$estimate_mau_lower_bound <- 1000
            fb_df$estimate_mau_upper_bound <- 1000
            fb_df$location_type <- "coordinates"
            fb_df$latitude <- df_i$latitude
            fb_df$longitude <- df_i$longitude
            fb_df$radius <- radius
            fb_df$radius_unit <- "kilometer"
            fb_df$param_id <- 1:nrow(parameters_df)
            
            SCRAPE_ALL_DATA <- FALSE
            
            Sys.sleep(1)
            
          }
        }
      }
      
      ## Check 2: Invalid Location
      if(!is.null(fb_radius_Xkm)){
        if(!is.null(fb_radius_Xkm$ERROR)){
          if(fb_radius_Xkm$ERROR %in% "Incorrect Location Format"){
            
            print("estimate_1_mau = invalid location; assigning NA to all params")
            
            fb_df <- data.frame(uid = rep(df_i$uid, nrow(parameters_df)))
            fb_df$ERROR <- "Incorrect Location Format"
            fb_df$param_id <- 1:nrow(parameters_df)
            
            SCRAPE_ALL_DATA <- FALSE
            
            Sys.sleep(1)
            
          }
        }
      }
      
      if(SCRAPE_ALL_DATA){
        print(paste0("Radius: ", radius, "; uid = ", uid_i, "; u/r = ", df_i$urban_rural))
        
        fb_df <- map_df(1:nrow(parameters_df), function(param_i){

          parameters_df_i <- parameters_df[param_i,]
          
          fb_df_i <- query_fb_marketing_api(location_type = "coordinates",
                                            latitude = df_i$latitude,
                                            longitude = df_i$longitude,
                                            radius = radius,
                                            radius_unit = "kilometer",
                                            education_statuses = parameters_df_i$education_statuses,
                                            behavior = parameters_df_i$behavior %>% str_split(",") %>% unlist(),
                                            interest = parameters_df_i$interest,
                                            gender = parameters_df_i$gender %>% str_split(",") %>% unlist(),
                                            age_min = parameters_df_i$age_min,
                                            age_max = parameters_df_i$age_max,
                                            version = version,
                                            creation_act = creation_act,
                                            token = token,
                                            sleep_time = 0.3)
          
          # Add variables if not null (ie, no error in calling function)
          if(!is.null(fb_df_i)){
            fb_df_i$param_id <- param_i
            fb_df_i$uid <- df_i$uid
          }
          
          return(fb_df_i)
        })
        
      }
      
      ## Export; Only export if scraped all parameters
      if(nrow(fb_df) == nrow(parameters_df)){
        print("Saved!")
        saveRDS(fb_df, OUT_PATH)
      }
      rm(fb_df)
      
      ## Sleep
      if(SCRAPE_ALL_DATA){
        
        sleep_time_check <- 0
        while(sleep_time_check < sleep_time_after_loc){

          if(sleep_time_check == 0){
            cat("Sleeping ")
            cat(round(sleep_time_after_loc))
            cat(" seconds: ")
          } else{
            cat("=")
          }
          
          Sys.sleep(1)
          sleep_time_check <- sleep_time_check + 1
          
        }
        
        cat("\n")
        
      }
      
      ## Change Key
      KEY_i <- KEY_i + 1
      if(KEY_i > N_KEYS) KEY_i <- 1
      
    }
  }
}





