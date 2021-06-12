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

# Load Coordinates -------------------------------------------------------------
df <- readRDS(file.path(secure_file_path, "Data", SURVEY_NAME,  "FinalData - PII", "GPS_uid_crosswalk.Rds"))

# Setup Credentials ------------------------------------------------------------
api_keys <- read.csv(file.path(webscraping_api_filepath, "api_keys.csv"), stringsAsFactors=F) %>%
  filter(Service == "facebook_marketing_ad",
         Details == "robmarty3@gmail.com")

token <- api_keys$Key[api_keys$Account %in% "token"]
creation_act <- api_keys$Key[api_keys$Account %in% "creation_act"]
version <- api_keys$Key[api_keys$Account %in% "version"]

# Parameters -------------------------------------------------------------------
library(httr)

demographics_df <- GET(
  
  "https://graph.facebook.com/v10.0/search",
  
  query=list(
    
    type='adTargetingCategory',
    
    class='demographics',
    
    access_token=token,
    
    limit=2000
    
  )) %>%content(as="text")%>%fromJSON%>%.[[1]]

interests_df <- GET(
  
  "https://graph.facebook.com/v10.0/search",
  
  query=list(
    
    type='adTargetingCategory',
    
    class='interests',
    
    access_token=token,
    
    limit=2000
    
  )) %>%content(as="text")%>%fromJSON%>%.[[1]]

behaviors_df <- GET(
  
  "https://graph.facebook.com/v10.0/search",
  
  query=list(
    
    type='adTargetingCategory',
    
    class='behaviors',
    
    access_token=token,
    
    limit=2000
    
  )) %>%content(as="text")%>%fromJSON%>%.[[1]]

# Export -----------------------------------------------------------------------

saveRDS(demographics_df,
        file.path(project_file_path, "Data", "Facebook",  "FinalData", "interests_demographics_behaviors_ids",
                  "demographics.Rds"))
write.csv(demographics_df %>% dplyr::select(id, name, type, description, audience_size),
        file.path(project_file_path, "Data", "Facebook",  "FinalData", "interests_demographics_behaviors_ids",
                  "demographics.csv"), row.names = F)

saveRDS(interests_df,
        file.path(project_file_path, "Data", "Facebook",  "FinalData", "interests_demographics_behaviors_ids",
                  "interests.Rds"))
write.csv(interests_df %>% dplyr::select(id, name, type, audience_size),
          file.path(project_file_path, "Data", "Facebook",  "FinalData", "interests_demographics_behaviors_ids",
                    "interests.csv"), row.names = F)

saveRDS(behaviors_df,
        file.path(project_file_path, "Data", "Facebook",  "FinalData", "interests_demographics_behaviors_ids",
                  "behaviors.Rds"))
write.csv(behaviors_df %>% dplyr::select(id, name, description, audience_size),
          file.path(project_file_path, "Data", "Facebook",  "FinalData", "interests_demographics_behaviors_ids",
                    "behaviors.csv"), row.names = F)


