# Merge Data Extracted from Facebook API

# Load Data ----------------------------------------------------------------------
param_df <- read.csv(file.path(data_dir, SURVEY_NAME, "FinalData", "Individual Datasets", "facebook_marketing_parameters.csv"), stringsAsFactors = F)

behaviors_df <- read.csv(file.path(data_dir, "Facebook Marketing", "FinalData", "interests_demographics_behaviors_ids", "behaviors.csv"), stringsAsFactors = F)
interests_df <- read.csv(file.path(data_dir, "Facebook Marketing", "FinalData", "interests_demographics_behaviors_ids", "interests.csv"), stringsAsFactors = F)

# Add behavior/interests names -------------------------------------------------
behaviors_df <- behaviors_df %>% mutate(id = id %>% as.character())
interests_df <- interests_df %>% mutate(id = id %>% as.character())

param_df <- param_df %>%
  dplyr::mutate(behavior = behavior %>% str_replace_all("'id'|\\{|:|\\}", ""),
                interest = interest %>% str_replace_all("'id'|\\{|:|\\}", ""))

beh_int_df <- map_df(1:nrow(param_df), function(i){
  
  param_df_i <- param_df[i,]
  
  beh_ids_i <- param_df_i$behavior %>% str_split(",") %>% unlist()
  beh_name_i <- behaviors_df$name[behaviors_df$id %in% beh_ids_i] %>% paste(collapse = "; ")
  
  int_ids_i <- param_df_i$interest %>% str_split(",") %>% unlist()
  int_name_i <- interests_df$name[interests_df$id %in% int_ids_i] %>% paste(collapse = "; ")
  
  out <- data.frame(behavior_name = beh_name_i,
                    interest_name = int_name_i)
  
  return(out)
})

param_df <- bind_cols(param_df, beh_int_df) %>%
  dplyr::mutate(behavior_name = behavior_name %>% as.character,
                interest_name = interest_name %>% as.character)
param_df$behavior_name[param_df$behavior_name %in% ""] <- NA
param_df$interest_name[param_df$interest_name %in% ""] <- NA

# Edit Eduction ----------------------------------------------------------------
# https://developers.facebook.com/docs/marketing-api/audiences/reference/advanced-targeting/#education_and_workplace
# 1: HIGH_SCHOOL
# 2: UNDERGRAD
# 3: ALUM
# 4: HIGH_SCHOOL_GRAD
# 5: SOME_COLLEGE
# 6: ASSOCIATE_DEGREE
# 7: IN_GRAD_SCHOOL
# 8: SOME_GRAD_SCHOOL
# 9: MASTER_DEGREE
# 10: PROFESSIONAL_DEGREE
# 11: DOCTORATE_DEGREE
# 12: UNSPECIFIED
# 13: SOME_HIGH_SCHOOL

param_df <- param_df %>%
  mutate(educ_status_simple = case_when(
    education_statuses == "1,4,13" ~ "High School",
    education_statuses == "2,5,6,7,8,9,10,11" ~ "More than high school",
    TRUE ~ education_statuses
  ))

# Parameter Name - One Variable ------------------------------------------------
param_df$educ_status_simple[!is.na(param_df$educ_status_simple)] <-
  paste0("Eduction: ", param_df$educ_status_simple[!is.na(param_df$educ_status_simple)])

param_df$interest_name[!is.na(param_df$interest_name)] <-
  paste0("Interests: ", param_df$interest_name[!is.na(param_df$interest_name)])

param_df$behavior_name[!is.na(param_df$behavior_name)] <-
  paste0("Behavior: ", param_df$behavior_name[!is.na(param_df$behavior_name)])

param_df$param_name <- NA
param_df$param_name[!is.na(param_df$educ_status_simple)] <- param_df$educ_status_simple[!is.na(param_df$educ_status_simple)]
param_df$param_name[!is.na(param_df$interest_name)]      <- param_df$interest_name[!is.na(param_df$interest_name)]
param_df$param_name[!is.na(param_df$behavior_name)]      <- param_df$behavior_name[!is.na(param_df$behavior_name)]

param_df$param_name[param_df$param_id %in% 1] <- "All"

# Param Name - Simplified Name -------------------------------------------------
param_df <- param_df %>%
  dplyr::mutate(param_name_clean = case_when(
    param_name == "Behavior: Facebook access (network type): 2G" ~ "Network access: 2G Network",
    param_name == "Behavior: Facebook access (network type): 3G" ~ "Network access: 3G Network",
    param_name == "Behavior: Facebook access (network type): 4G" ~ "Network access: 4G Network",
    param_name == "Behavior: Facebook access (network type): Wifi" ~ "Network access: Wifi Network",
    
    param_name == "Behavior: Facebook access (mobile): Android devices" ~ "Mobile OS: Android devices",
    param_name == "Behavior: Facebook access (mobile): Apple (iOS) devices" ~ "Mobile OS: iOS",
    param_name == "Behavior: Facebook access (mobile): Windows phones" ~ "Mobile OS: Windows phones",
    
    param_name == "Behavior: Facebook access (mobile): iPhone X; Facebook access (mobile): iPhone XS; Facebook access (mobile): iPhone XS Max; Facebook access (mobile): iPhone XR" ~ 
      "High-end phones: Apple iPhone X/XS/XS Max/XR",
    param_name == "Behavior: Facebook access (mobile): iPhone 8; Facebook access (mobile): iPhone 8 Plus; Facebook access (mobile): iPhone X; Facebook access (mobile): iPhone XS; Facebook access (mobile): iPhone XS Max; Facebook access (mobile): iPhone XR" ~ 
      "High-end phones: Apple iPhone X/XS/XS Max/XR/8/8 Plus",
    param_name == "Behavior: Owns: Galaxy S9+" ~ 
      "High-end phones: Samsung Galaxy phone S9+",
    param_name == "Behavior: Owns: Galaxy S8; Owns: Galaxy S8+; Owns: Galaxy S9; Owns: Galaxy S9+" ~ 
      "High-end phones: Samsung Galaxy phone S8/S8+/S9/S9+",
    param_name == "Behavior: Owns: Galaxy S8; Owns: Galaxy S8+; Facebook access (mobile): iPhone 8; Facebook access (mobile): iPhone 8 Plus; Facebook access (mobile): iPhone X; Owns: Galaxy S9; Owns: Galaxy S9+; Facebook access (mobile): iPhone XS; Facebook access (mobile): iPhone XS Max; Facebook access (mobile): iPhone XR" ~ 
      "High-end phones: Samsung Galaxy phone S8/S8+/S9/S9+ or Apple iPhone X/XS/XS Max/XR",

    param_name == "Behavior: Facebook access (mobile): all mobile devices" ~ "Other device types: All mobile devices",
    param_name == "Behavior: Facebook access (mobile): feature phones" ~ "Other device types: Feature phones",
    param_name == "Behavior: Facebook access (mobile): smartphones and tablets" ~ "Other device types: Smartphones and tablets",
    param_name == "Behavior: Facebook access (mobile): tablets" ~ "Other device types: Tablets",

    param_name == "Behavior: Owns: Cherry Mobile" ~ "Other device types: Cherry mobile",
    param_name == "Behavior: Owns: VIVO devices" ~ "Other device types: VIVO mobile devices",
    param_name == "Behavior: Owns: Huawei" ~ "Other device types: Huawei mobile devices",
    param_name == "Behavior: Owns: Oppo" ~ "Other device types: Oppo mobile devices",
    param_name == "Behavior: Owns: Cherry Mobile; Owns: Oppo; Owns: VIVO devices" ~ "Other device types: Oppo/VIVO/Cherry devices",
    param_name == "Behavior: Facebook access (mobile): Samsung Android mobile devices" ~ "Other device types: Samsung Android devices",
    param_name == "Behavior: Facebook access: older devices and OS" ~ "Other device types: older devices and OS",
    
    param_name == "Behavior: Frequent Travelers" ~ "Behavior: Frequent travelers",
    param_name == "Behavior: Frequent international travelers" ~ "Behavior: Frequent international travelers",
    param_name == "Behavior: Small business owners" ~ "Behavior: Small business owners",
    param_name == "Behavior: Technology early adopters" ~ "Behavior: Technology early adopters",
    
    param_name == "Interests: Gambling" ~ "Interests: Gambling",
    param_name == "Interests: Fitness and wellness" ~ "Interests: Fitness and wellness",
    param_name == "Interests: Luxury goods" ~ "Interests: Luxury goods",
    TRUE ~ param_name
  ))

param_df$param_category <- param_df$param_name_clean %>% str_replace_all(":.*", "")
param_df$param_name_simple <- param_df$param_name_clean %>% str_replace_all(".*:", "") %>% str_squish()

# Export -----------------------------------------------------------------------
write.csv(param_df, file.path(data_dir, SURVEY_NAME, "FinalData", "Individual Datasets", "facebook_marketing_parameters_clean.csv"), row.names = F)
saveRDS(param_df, file.path(data_dir, SURVEY_NAME, "FinalData", "Individual Datasets", "facebook_marketing_parameters_clean.Rds"))




