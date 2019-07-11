# Merge Survey Data with Pakistan Boundaries (Union Councils)
# Creates a shapefile of union councils with the survey data

# Load Data ====================================================================
#### Union Council Shapefile 
uc_sdf <- readOGR(dsn=file.path(project_file_path, "Data", "RawData", "Pakistan Boundaries", "HDX"),
                  layer="Union_Council")

uc_sdf@data$PROVINCE <- uc_sdf$PROVINCE %>% as.character %>% tolower 
uc_sdf@data$DISTRICT <- uc_sdf$DISTRICT %>% as.character %>% tolower
uc_sdf@data$TEHSIL <- uc_sdf$TEHSIL %>% as.character %>% tolower
uc_sdf@data$UC <- uc_sdf$UC %>% as.character %>% tolower

#### Union Council Survey Data
survey_uc_df <- read.csv(file.path(project_file_path, "Data", "RawData", "NSER", "Union Council Level Dataset","nser_data_uc.csv"))

survey_uc_df$full_address <- paste(survey_uc_df$unioncouncil_name,
                                   survey_uc_df$tehsil_name,
                                   survey_uc_df$district_name,
                                   "punjab",
                                   "pakistan",
                                   sep=", ") %>% tolower %>% str_squish()

survey_uc_df$district_name <- survey_uc_df$district_name %>% as.character %>% tolower
survey_uc_df$tehsil_name <- survey_uc_df$tehsil_name %>% as.character %>% tolower
survey_uc_df$unioncouncil_name <- survey_uc_df$unioncouncil_name %>% as.character %>% tolower

survey_uc_df$district_name_original <- survey_uc_df$district_name
survey_uc_df$tehsil_name_original <- survey_uc_df$tehsil_name
survey_uc_df$unioncouncil_name_original <- survey_uc_df$unioncouncil_name

#### Google API Data
google_geocoder_output <- read.csv(file.path(project_file_path, "Data", "FinalData", "UC Google Geocoder Output", "uc_google_geocoder_output.csv"))
names(google_geocoder_output)[-10] <- paste0("google_", names(google_geocoder_output)[-10]) 

survey_uc_df <- merge(survey_uc_df, google_geocoder_output, by.x="full_address", by.y="address_input") 

# Clean Survey Data to Match Shapefile Patterns ================================
# Modify data in survey data so that it matches patterns in shapefile

# 1. When abbreviating number, shapefile uses "no." while survey data uses "no"
survey_uc_df$unioncouncil_name <- gsub(" no "," no. ", survey_uc_df$unioncouncil_name)

# 2. Remove extra white space ("word1    word2" -> "word1 word2")
survey_uc_df$unioncouncil_name <- gsub("\\s+"," ", survey_uc_df$unioncouncil_name)

# Change Names in Survey to Match with Shapefile ===============================

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
#####                        *** DISTRICTS *** 
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

#### Surveys without a match
unique(survey_uc_df$district_name)[!(unique(survey_uc_df$district_name) %in% uc_sdf@data$DISTRICT)]

#### Manually Change Names in Survey Data
survey_uc_df$district_name[survey_uc_df$district_name_original == "leiah"] <- "layyah" # https://en.wikipedia.org/wiki/Layyah
survey_uc_df$district_name[survey_uc_df$district_name_original == "nankana sahib"] <- "sheikhupura" # https://en.wikipedia.org/wiki/Layyah

survey_uc_df$district_name_howfoundmatch <- "matched names"
survey_uc_df$district_name_howfoundmatch[survey_uc_df$district_name_original == "sheikhupura"] <- "redistricting"

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
#####                          *** TEHSILS *** 
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

#### Manually Change Names in Survey Data
survey_uc_df$tehsil_name[survey_uc_df$tehsil_name_original == "leiah"] <- "layyah"
survey_uc_df$tehsil_name[survey_uc_df$tehsil_name_original == "arif wala"] <- "arifwala"
survey_uc_df$tehsil_name[survey_uc_df$tehsil_name_original == "ahmadpur sial"] <- "ahmedpur sial"
survey_uc_df$tehsil_name[survey_uc_df$tehsil_name_original == "chak jhumra"] <- "chak jumra"
survey_uc_df$tehsil_name[survey_uc_df$tehsil_name_original == "choubara"] <- "chaubara"
survey_uc_df$tehsil_name[survey_uc_df$tehsil_name_original == "faisalabad sadar"] <- "faisalabad saddar"
survey_uc_df$tehsil_name[survey_uc_df$tehsil_name_original == "fateh jang"] <- "fatehjang"
survey_uc_df$tehsil_name[survey_uc_df$tehsil_name_original == "gulberg town"] <- "gulbarg town"
survey_uc_df$tehsil_name[survey_uc_df$tehsil_name_original == "hasan abdal"] <- "hassanabdal"
survey_uc_df$tehsil_name[survey_uc_df$tehsil_name_original == "jahanian"] <- "jehanian"
survey_uc_df$tehsil_name[survey_uc_df$tehsil_name_original == "kahror pacca"] <- "kehror pacca"
survey_uc_df$tehsil_name[survey_uc_df$tehsil_name_original == "lahore city"] <- "lahor" # maybe also: lahore cantt
survey_uc_df$tehsil_name[survey_uc_df$tehsil_name_original == "nowshera virkan"] <- "naushera virkan"
survey_uc_df$tehsil_name[survey_uc_df$tehsil_name_original == "qaidabad"] <- "quaidabad"
survey_uc_df$tehsil_name[survey_uc_df$tehsil_name_original == "summundri"] <- "samundri"
survey_uc_df$tehsil_name[survey_uc_df$tehsil_name_original == "tala gang"] <- "talagang"
survey_uc_df$tehsil_name[survey_uc_df$tehsil_name_original == "tandlian wala"] <- "tandlianwala"
survey_uc_df$tehsil_name[survey_uc_df$tehsil_name_original == "rawalpindi r"] <- "rawalpindi tehsil"

#### If can't find name match, find lat/lon of tehsil
unmatched_ADMs_survey <- unique(survey_uc_df$tehsil_name)[!(unique(survey_uc_df$tehsil_name) %in% uc_sdf$TEHSIL)]
unmatched_ADMs_shp <- unique(uc_sdf$TEHSIL)[!(unique(uc_sdf$TEHSIL) %in% survey_uc_df$tehsil_name)]

survey_uc_df$tehsil_lat <- NA
survey_uc_df$tehsil_lon <- NA

survey_uc_df$tehsil_lat[survey_uc_df$tehsil_name == "18-hazari"] <- 31.167072 
survey_uc_df$tehsil_lon[survey_uc_df$tehsil_name == "18-hazari"] <- 72.089819

survey_uc_df$tehsil_lat[survey_uc_df$tehsil_name %in% c("allam iqbal town","allam town",  "allama iqbal town", "allama iqbal twon")] <- 31.511111
survey_uc_df$tehsil_lon[survey_uc_df$tehsil_name %in% c("allam iqbal town","allam town", "allama iqbal town", "allama iqbal twon")] <- 74.283889

survey_uc_df$tehsil_lat[survey_uc_df$tehsil_name == "aziz bhatti town"] <- 32.116667 
survey_uc_df$tehsil_lon[survey_uc_df$tehsil_name == "aziz bhatti town"] <- 72.65

survey_uc_df$tehsil_lat[survey_uc_df$tehsil_name == "aroop town"] <- 32.220518
survey_uc_df$tehsil_lon[survey_uc_df$tehsil_name == "aroop town"] <- 74.219534

survey_uc_df$tehsil_lat[survey_uc_df$tehsil_name == "data gb town"] <- 31.566667 # this is a union council in here: https://en.wikipedia.org/wiki/Qila_Gujar_Singh
survey_uc_df$tehsil_lon[survey_uc_df$tehsil_name == "data gb town"] <- 74.333333

survey_uc_df$tehsil_lat[survey_uc_df$tehsil_name == "d.g khan (tribal area)"] <- 30.033056 
survey_uc_df$tehsil_lon[survey_uc_df$tehsil_name == "d.g khan (tribal area)"] <- 70.64

survey_uc_df$tehsil_lat[survey_uc_df$tehsil_name == "dina"] <- 33.028333 
survey_uc_df$tehsil_lon[survey_uc_df$tehsil_name == "dina"] <- 73.601111

survey_uc_df$tehsil_lat[survey_uc_df$tehsil_name == "faisalabad"] <- 31.416667
survey_uc_df$tehsil_lon[survey_uc_df$tehsil_name == "faisalabad"] <- 73.091111

survey_uc_df$tehsil_lat[survey_uc_df$tehsil_name == "gujar khan"] <- 33.253 
survey_uc_df$tehsil_lon[survey_uc_df$tehsil_name == "gujar khan"] <- 73.304

survey_uc_df$tehsil_lat[survey_uc_df$tehsil_name == "gujranwala"] <- 32.156667 
survey_uc_df$tehsil_lon[survey_uc_df$tehsil_name == "gujranwala"] <- 74.19

survey_uc_df$tehsil_lat[survey_uc_df$tehsil_name == "hazro"] <- 33.90927 
survey_uc_df$tehsil_lon[survey_uc_df$tehsil_name == "hazro"] <- 72.492

survey_uc_df$tehsil_lat[survey_uc_df$tehsil_name == "kahuta"] <- 33.583333 
survey_uc_df$tehsil_lon[survey_uc_df$tehsil_name == "kahuta"] <- 73.383333

survey_uc_df$tehsil_lat[survey_uc_df$tehsil_name == "kallar kahar"] <- 32.783333 
survey_uc_df$tehsil_lon[survey_uc_df$tehsil_name == "kallar kahar"] <- 72.7

survey_uc_df$tehsil_lat[survey_uc_df$tehsil_name == "kallar sayaddan"] <- 33.414444 
survey_uc_df$tehsil_lon[survey_uc_df$tehsil_name == "kallar sayaddan"] <- 73.378611

survey_uc_df$tehsil_lat[survey_uc_df$tehsil_name == "khiali shah pur town"] <- 32.121 
survey_uc_df$tehsil_lon[survey_uc_df$tehsil_name == "khiali shah pur town"] <- 74.173

survey_uc_df$tehsil_lat[survey_uc_df$tehsil_name == "kot momin"] <- 32.188333 
survey_uc_df$tehsil_lon[survey_uc_df$tehsil_name == "kot momin"] <- 73.028611

survey_uc_df$tehsil_lat[survey_uc_df$tehsil_name == "kot radha kishen"] <- 31.1725
survey_uc_df$tehsil_lon[survey_uc_df$tehsil_name == "kot radha kishen"] <- 74.099722

survey_uc_df$tehsil_lat[survey_uc_df$tehsil_name == "kotli sattian"] <- 33.807593
survey_uc_df$tehsil_lon[survey_uc_df$tehsil_name == "kotli sattian"] <- 73.525482

survey_uc_df$tehsil_lat[survey_uc_df$tehsil_name == "lahore cantt"] <- 31.516667
survey_uc_df$tehsil_lon[survey_uc_df$tehsil_name == "lahore cantt"] <- 74.383333

survey_uc_df$tehsil_lat[survey_uc_df$tehsil_name == "malakwal"] <- 32.553056
survey_uc_df$tehsil_lon[survey_uc_df$tehsil_name == "malakwal"] <- 73.206667

survey_uc_df$tehsil_lat[survey_uc_df$tehsil_name == "muridke"] <- 31.802
survey_uc_df$tehsil_lon[survey_uc_df$tehsil_name == "muridke"] <- 74.255

survey_uc_df$tehsil_lat[survey_uc_df$tehsil_name == "murree"] <- 33.904167
survey_uc_df$tehsil_lon[survey_uc_df$tehsil_name == "murree"] <- 73.390278

survey_uc_df$tehsil_lat[survey_uc_df$tehsil_name == "nandi pur town"] <- 32.250117
survey_uc_df$tehsil_lon[survey_uc_df$tehsil_name == "nandi pur town"] <- 74.267271

survey_uc_df$tehsil_lat[survey_uc_df$tehsil_name == "nashter town"] <- 31.478565 # maybe double check
survey_uc_df$tehsil_lon[survey_uc_df$tehsil_name == "nashter town"] <- 74.352312

survey_uc_df$tehsil_lat[survey_uc_df$tehsil_name == "qila dedar singh town"] <- 32.8
survey_uc_df$tehsil_lon[survey_uc_df$tehsil_name == "qila dedar singh town"] <- 74.1

survey_uc_df$tehsil_lat[survey_uc_df$tehsil_name == "rajanpur (tribal area)"] <- 29.104167
survey_uc_df$tehsil_lon[survey_uc_df$tehsil_name == "rajanpur (tribal area)"] <- 70.324722

survey_uc_df$tehsil_lat[survey_uc_df$tehsil_name == "ravi town"] <- 31.616667
survey_uc_df$tehsil_lon[survey_uc_df$tehsil_name == "ravi town"] <- 74.3

survey_uc_df$tehsil_lat[survey_uc_df$tehsil_name == "rawalpindi"] <- 33.6
survey_uc_df$tehsil_lon[survey_uc_df$tehsil_name == "rawalpindi"] <- 73.033333

survey_uc_df$tehsil_lat[survey_uc_df$tehsil_name == "samanabad town"] <- 31.536198
survey_uc_df$tehsil_lon[survey_uc_df$tehsil_name == "samanabad town"] <- 74.299013

survey_uc_df$tehsil_lat[survey_uc_df$tehsil_name == "sambrial"] <- 32.16
survey_uc_df$tehsil_lon[survey_uc_df$tehsil_name == "sambrial"] <- 74.4

survey_uc_df$tehsil_lat[survey_uc_df$tehsil_name == "sangla hill"] <- 31.713333
survey_uc_df$tehsil_lon[survey_uc_df$tehsil_name == "sangla hill"] <- 73.374444

survey_uc_df$tehsil_lat[survey_uc_df$tehsil_name == "shah kot"] <- 31.573333
survey_uc_df$tehsil_lon[survey_uc_df$tehsil_name == "shah kot"] <- 73.480556

survey_uc_df$tehsil_lat[survey_uc_df$tehsil_name == "shaker garh"] <- 32.262778
survey_uc_df$tehsil_lon[survey_uc_df$tehsil_name == "shaker garh"] <- 75.158333

survey_uc_df$tehsil_lat[survey_uc_df$tehsil_name == "shalimar town"] <- 31.585833 
survey_uc_df$tehsil_lon[survey_uc_df$tehsil_name == "shalimar town"] <- 74.381944

survey_uc_df$tehsil_lat[survey_uc_df$tehsil_name == "sharak pur"] <- 31.463333
survey_uc_df$tehsil_lon[survey_uc_df$tehsil_name == "sharak pur"] <- 74.1

survey_uc_df$tehsil_lat[survey_uc_df$tehsil_name == "taxila"] <- 33.745833
survey_uc_df$tehsil_lon[survey_uc_df$tehsil_name == "taxila"] <- 72.7875

survey_uc_df$tehsil_lat[survey_uc_df$tehsil_name == "wahga town"] <- 31.604722
survey_uc_df$tehsil_lon[survey_uc_df$tehsil_name == "wahga town"] <- 74.573056

survey_uc_df$tehsil_lat[survey_uc_df$tehsil_name == "zafarwal"] <- 32.21 
survey_uc_df$tehsil_lon[survey_uc_df$tehsil_name == "zafarwal"] <- 74.54

survey_uc_df_tehsillatlon <- subset(survey_uc_df, select=c(tehsil_name, tehsil_lat, tehsil_lon)) %>% unique
survey_uc_df_tehsillatlon <- survey_uc_df_tehsillatlon[!is.na(survey_uc_df_tehsillatlon$tehsil_lat),]
coordinates(survey_uc_df_tehsillatlon) <- ~tehsil_lon+tehsil_lat
crs(survey_uc_df_tehsillatlon) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

survey_uc_df_tehsillatlon_OVER_uc_sdf <- over(survey_uc_df_tehsillatlon, uc_sdf)
survey_uc_df_tehsillatlon$tehsil_sdf <- survey_uc_df_tehsillatlon_OVER_uc_sdf$TEHSIL 
survey_uc_df_tehsillatlon <- survey_uc_df_tehsillatlon@data[!is.na(survey_uc_df_tehsillatlon$tehsil_name),]

survey_uc_df <- merge(survey_uc_df, survey_uc_df_tehsillatlon, by="tehsil_name", all.x=T)

survey_uc_df$tehsil_name_howfoundmatch <- "matched names"
survey_uc_df$tehsil_name_howfoundmatch[!is.na(survey_uc_df$tehsil_sdf)] <- "found coordinates of tehsil name, matched coordinates with tehsil polygon"

survey_uc_df$tehsil_name[!is.na(survey_uc_df$tehsil_sdf)] <- survey_uc_df$tehsil_sdf[!is.na(survey_uc_df$tehsil_sdf)]

# Export =======================================================================
saveRDS(survey_uc_df, file.path(final_data_file_path, "UC with NSER Data", "individual_files", "01_districttehsil.Rds"))
saveRDS(survey_uc_df, file.path(final_data_file_path, "UC with NSER Data", "individual_files", "uc_hdx_shapefile_clean.Rds"))

