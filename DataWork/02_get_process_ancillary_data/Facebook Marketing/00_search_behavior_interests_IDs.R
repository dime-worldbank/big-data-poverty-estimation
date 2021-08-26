# Facebook Marketing API: Parameter Datasets

# From the Facebook Marketing API, we can extract MAU/DAU according to demographic,
# interests, and behavior parameters. This code creates datasets of the parameters
# with the associated Facebook ID for the parameter. We then use these IDs when
# scraping data.

# Setup Credentials ------------------------------------------------------------
api_keys <- read.csv(file.path(webscraping_api_filepath, "api_keys.csv"), stringsAsFactors=F) %>%
  filter(Service == "facebook_marketing_ad",
         Details == "robmarty3@gmail.com")

token <- api_keys$Key[api_keys$Account %in% "token"]
creation_act <- api_keys$Key[api_keys$Account %in% "creation_act"]
version <- api_keys$Key[api_keys$Account %in% "version"]

# Parameters -------------------------------------------------------------------
demographics_df <- GET(
  "https://graph.facebook.com/v10.0/search",
  query=list(
    type='adTargetingCategory',
    class='demographics',
    access_token=token,
    limit=2000
  )) %>% content(as="text") %>% fromJSON %>%. [[1]]

interests_df <- GET(
  "https://graph.facebook.com/v10.0/search",
  query=list(
    type='adTargetingCategory',
    class='interests',
    access_token=token,
    limit=2000
  )) %>% content(as="text") %>% fromJSON %>% .[[1]]

behaviors_df <- GET(
  "https://graph.facebook.com/v10.0/search",
  query=list(
    type='adTargetingCategory',
    class='behaviors',
    access_token=token,
    limit=2000
  )) %>% content(as="text") %>% fromJSON %>% .[[1]]

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

