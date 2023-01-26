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

# Load Coordinates -------------------------------------------------------------
df <- readRDS(file.path(data_dir, SURVEY_NAME, "FinalData", "Individual Datasets", 
                        "survey_socioeconomic.Rds"))

# Setup Credentials ------------------------------------------------------------
api_keys <- read.csv(file.path(api_key_dir, "api_keys.csv"), stringsAsFactors=F) %>%
  filter(Service == "facebook_marketing_ad")

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
sleep_time_after_loc <- sleep_time_after_loc - 130

sleep_time_after_loc <- sleep_time_after_loc / N_KEYS

# Scrape Data ------------------------------------------------------------------
country_isos <- df$iso2 %>% unique()

country_isos <- rep(country_isos, 5)

#### Loop over countries
KEY_i <- 1
for(iso_i in country_isos){
  print(iso_i)
  
  OUT_PATH <- file.path(fb_marketing_dir,  "FinalData", "country_level_mau", "Individual Datasets", 
                        paste0("fb_",iso_i,".Rds"))
  
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
    
    fb_df <- map_df(1:nrow(parameters_df), function(param_i){
      parameters_df_i <- parameters_df[param_i,]
      
      fb_df_i <- query_fb_marketing_api(location_type = "country",
                                        country_iso2 = iso_i,
                                        education_statuses = parameters_df_i$education_statuses,
                                        behavior = parameters_df_i$behavior %>% str_split(",") %>% unlist(),
                                        interest = parameters_df_i$interest,
                                        gender = parameters_df_i$gender %>% str_split(",") %>% unlist(),
                                        age_min = parameters_df_i$age_min,
                                        age_max = parameters_df_i$age_max,
                                        version = version,
                                        creation_act = creation_act,
                                        token = token,
                                        sleep_time = 0.1)
      
      # Add variables if not null (ie, no error in calling function)
      if(!is.null(fb_df_i)){
        fb_df_i$param_id <- param_i
      }
      
      return(fb_df_i)
    })
    
    ## Export; Only export if scraped all parameters
    if(nrow(fb_df) == nrow(parameters_df)){
      print("Saved!")
      saveRDS(fb_df, OUT_PATH)
    }
    rm(fb_df)
    
    ## Sleep
    Sys.sleep(sleep_time_after_loc)
    
    ## Change Key
    KEY_i <- KEY_i + 1
    if(KEY_i > N_KEYS) KEY_i <- 1
    
  }
}


