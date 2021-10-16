# Facebook Marketing API: Parameter Datasets

# From the Facebook Marketing API, we can extract MAU/DAU according to demographic,
# interests, and behavior parameters. This code creates datasets of the parameters
# with the associated Facebook ID for the parameter. We then use these IDs when
# scraping data.

# Setup Credentials ------------------------------------------------------------
api_keys <- read.csv(file.path(api_key_dir, "api_keys.csv"), stringsAsFactors=F) %>%
  filter(Service == "facebook_marketing_ad",
         Details == "ramarty92@gmail.com")

token <- api_keys$Key[api_keys$Account %in% "token"]
creation_act <- api_keys$Key[api_keys$Account %in% "creation_act"]
version <- api_keys$Key[api_keys$Account %in% "version"]

# Parameters -------------------------------------------------------------------
demographics_df <- get_fb_parameters(class = "demographics",
                                     version = version,
                                     token = token)

interests_df <- get_fb_parameters(class = "interests",
                                  version = version,
                                  token = token)

behaviors_df <- get_fb_parameters(class = "behaviors",
                                  version = version,
                                  token = token)

# Export -----------------------------------------------------------------------
saveRDS(demographics_df,
        file.path(data_dir, "Facebook Marketing",  "FinalData", "interests_demographics_behaviors_ids",
                  "demographics.Rds"))
write.csv(demographics_df %>% dplyr::select(id, name, type, description, audience_size),
          file.path(data_dir, "Facebook Marketing",  "FinalData", "interests_demographics_behaviors_ids",
                    "demographics.csv"), row.names = F)

saveRDS(interests_df,
        file.path(data_dir, "Facebook Marketing",  "FinalData", "interests_demographics_behaviors_ids",
                  "interests.Rds"))
write.csv(interests_df %>% dplyr::select(id, name, type, audience_size),
          file.path(data_dir, "Facebook Marketing",  "FinalData", "interests_demographics_behaviors_ids",
                    "interests.csv"), row.names = F)

saveRDS(behaviors_df,
        file.path(data_dir, "Facebook Marketing",  "FinalData", "interests_demographics_behaviors_ids",
                  "behaviors.Rds"))
write.csv(behaviors_df %>% dplyr::select(id, name, description, audience_size),
          file.path(data_dir, "Facebook Marketing",  "FinalData", "interests_demographics_behaviors_ids",
                    "behaviors.csv"), row.names = F)

