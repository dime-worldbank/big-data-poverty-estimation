# Create Parameter Dataframe to Scrape

# Parameters -------------------------------------------------------------------
AGE_MIN = 18
AGE_MAX = 65
FACEBOOK_POSITIONS = "'feed','instant_article','instream_video','marketplace'"
RADIUS_KM = 10

# All
parameters_df_all <- data.frame(
  radius_km = RADIUS_KM,
  gender = "1,2",
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
  gender = "1,2",
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
  gender = "1,2",
  age_min = AGE_MIN,
  age_max = AGE_MAX,
  facebook_positions = FACEBOOK_POSITIONS,
  device_platforms = "'mobile','desktop'",
  publisher_platforms = "'facebook','messenger'",
  messenger_positions = "'messenger_home'",
  user_os = c("iOS",
              "Android",
              #"'iOS_ver_2.0_to_3.0'",
              #"'iOS_ver_4.0_to_5.0'",
              #"'iOS_ver_6.0_to_7.0'",
              #"'iOS_ver_8.0_and_above'",
              "iOS_ver_9.0_and_above",
              #"'Android_ver_2.0_to_3.2'",
              #"'Android_ver_4.0_to_5.1'",
              #"'Android_ver_6.0_to_7.1'",
              #"'Android_ver_7.0_and_above'",
              "Android_ver_8.0_and_above"),
  stringsAsFactors = F
)

# Wireless carrier
parameters_df_carrier <- data.frame(
  radius_km = RADIUS_KM,
  gender = "1,2",
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
  gender = "1,2",
  age_min = AGE_MIN,
  age_max = AGE_MAX,
  facebook_positions = FACEBOOK_POSITIONS,
  device_platforms = "'mobile','desktop'",
  publisher_platforms = "'facebook','messenger'",
  messenger_positions = "'messenger_home'",
  behavior = c(
    # Network access
    "6017253486583", # Facebook access (network type): 2G
    "6017253511583", # Facebook access (network type): 3G
    "6017253531383", # Facebook access (network type): 4G
    "6015235495383", # Facebook access (network type): WiFi
    
    # Mobile OS
    "6004386044572", # Facebook access (mobile): Android devices
    "6004384041172", # Facebook access (mobile): Apple (iOS) devices
    #"6004385895772", # Facebook access (mobile): Windows phones
    
    # High end phones
    "6092512462983,6120699687383,6120699721983,6120699725783", # Facebook access (mobile): iPhone X/XS/XS Max/XR
    "6092512462983,6120699687383,6120699721983,6120699725783,6092512412783,6092512424583", # iPhone X/XS/XS Max/XR/8/8 Plus
    "6106224431383", # Owns: Galaxy S9+
    "6075237200983,6075237226583,6106223987983,6106224431383", #Samsung Galaxy phone S8/S8+/S9/S9+ 
    "6075237200983,6075237226583,6106223987983,6106224431383,6092512462983,6120699687383,6120699721983,6120699725783,6092512412783,6092512424583", # iPhone X/XS/XS Max/XR/8/8 Plus or Samsung Galaxy phone S8/S8+/S9/S9+ 
    
    # Other device types
    "6004382299972", # Facebook access (mobile): all mobile devices
    #"6004383149972", # Facebook access (mobile): feature phones
    "6004383049972", # Facebook access (mobile): smartphones and tablets
    "6016286626383", # Facebook access (mobile): tablets
    #"6023460590583", # Owns: Cherry Mobile
    "6056265212183", # Owns: VIVO devices
    "6011390261383", # Owns: Huawei
    "6056265200983", # Owns: Oppo
    "6056265200983,6056265212183,6023460590583", # Oppo/Vivo/Cherry
    "6004386010572", # Facebook access (mobile): Samsung Android mobile devices
    
    # OTHER
    "6002714895372", # Frequent Travelers
    "6022788483583", # Frequent international travelers
    "6002714898572", # Small business owners
    "6003808923172", # Technology early adopters
    "6004854404172" # Facebook access: older devices and OS
  ), 
  stringsAsFactors = F
)

# Interests
parameters_df_interests <- data.frame(
  radius_km = RADIUS_KM,
  gender = "1,2",
  age_min = AGE_MIN,
  age_max = AGE_MAX,
  facebook_positions = FACEBOOK_POSITIONS,
  device_platforms = "'mobile','desktop'",
  publisher_platforms = "'facebook','messenger'",
  messenger_positions = "'messenger_home'",
  interest = c("6003012317397", # Gambling
               "6003384248805", # Fitness and wellness
               "6007828099136", # Luxury Goods
               "6003346592981", # Online Shopping
               "6004160395895", # Travel
               "6003436950375", # Restaurants
               "6003430696269",  # Tourism
               "6003263791114" # Shopping
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

# Export -----------------------------------------------------------------------
saveRDS(parameters_df, file.path(data_dir, "Facebook Marketing", "FinalData", "facebook_marketing_parameters.Rds"))

