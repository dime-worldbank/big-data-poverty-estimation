# Merge Survey Data with Pakistan Boundaries (Union Councils)

# Load Data ====================================================================
#### NSER Survey
survey_uc_df <- readRDS(file.path(final_data_file_path, "UC with NSER Data", "individual_files", "nsersurvey_blank.Rds"))

survey_uc_df$district_name_hdx <- survey_uc_df$district_name_original %>% as.character %>% tolower
survey_uc_df$tehsil_name_hdx <- survey_uc_df$tehsil_name_original %>% as.character %>% tolower
survey_uc_df$unioncouncil_name_hdx <- survey_uc_df$unioncouncil_name_original %>% as.character %>% tolower

#### HDX Union Council Shapefile 
uc_sdf <- readOGR(dsn=file.path(project_file_path, "Data", "RawData", "Pakistan Boundaries", "HDX"),
                  layer="Union_Council")

uc_sdf@data$PROVINCE <- uc_sdf$PROVINCE %>% as.character %>% tolower 
uc_sdf@data$DISTRICT <- uc_sdf$DISTRICT %>% as.character %>% tolower
uc_sdf@data$TEHSIL <- uc_sdf$TEHSIL %>% as.character %>% tolower
uc_sdf@data$UC <- uc_sdf$UC %>% as.character %>% tolower

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
######                         *** DISTRICTS ***                         ######
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

#### Surveys without a match
unique(survey_uc_df$district_name_hdx)[!(unique(survey_uc_df$district_name_hdx) %in% uc_sdf@data$DISTRICT)]

#### Manually Change Names in Survey Data
survey_uc_df$district_name_hdx[survey_uc_df$district_name_original == "leiah"] <- "layyah" # https://en.wikipedia.org/wiki/Layyah
survey_uc_df$district_name_hdx[survey_uc_df$district_name_original == "nankana sahib"] <- "sheikhupura" # https://en.wikipedia.org/wiki/Nankana_Sahib_District; redistricting

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
######                         *** TEHSILS ***                           ######
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

# 1. Manually Change Names in Survey Data --------------------------------------
survey_uc_df$tehsil_name_hdx[survey_uc_df$tehsil_name_original == "leiah"] <- "layyah"
survey_uc_df$tehsil_name_hdx[survey_uc_df$tehsil_name_original == "arif wala"] <- "arifwala"
survey_uc_df$tehsil_name_hdx[survey_uc_df$tehsil_name_original == "ahmadpur sial"] <- "ahmedpur sial"
survey_uc_df$tehsil_name_hdx[survey_uc_df$tehsil_name_original == "chak jhumra"] <- "chak jumra"
survey_uc_df$tehsil_name_hdx[survey_uc_df$tehsil_name_original == "choubara"] <- "chaubara"
survey_uc_df$tehsil_name_hdx[survey_uc_df$tehsil_name_original == "faisalabad sadar"] <- "faisalabad saddar"
survey_uc_df$tehsil_name_hdx[survey_uc_df$tehsil_name_original == "fateh jang"] <- "fatehjang"
survey_uc_df$tehsil_name_hdx[survey_uc_df$tehsil_name_original == "gulberg town"] <- "gulbarg town"
survey_uc_df$tehsil_name_hdx[survey_uc_df$tehsil_name_original == "hasan abdal"] <- "hassanabdal"
survey_uc_df$tehsil_name_hdx[survey_uc_df$tehsil_name_original == "jahanian"] <- "jehanian"
survey_uc_df$tehsil_name_hdx[survey_uc_df$tehsil_name_original == "kahror pacca"] <- "kehror pacca"
survey_uc_df$tehsil_name_hdx[survey_uc_df$tehsil_name_original == "lahore city"] <- "lahor" # maybe also: lahore cantt
survey_uc_df$tehsil_name_hdx[survey_uc_df$tehsil_name_original == "nowshera virkan"] <- "naushera virkan"
survey_uc_df$tehsil_name_hdx[survey_uc_df$tehsil_name_original == "qaidabad"] <- "quaidabad"
survey_uc_df$tehsil_name_hdx[survey_uc_df$tehsil_name_original == "summundri"] <- "samundri"
survey_uc_df$tehsil_name_hdx[survey_uc_df$tehsil_name_original == "tala gang"] <- "talagang"
survey_uc_df$tehsil_name_hdx[survey_uc_df$tehsil_name_original == "tandlian wala"] <- "tandlianwala"
survey_uc_df$tehsil_name_hdx[survey_uc_df$tehsil_name_original == "rawalpindi r"] <- "rawalpindi tehsil"

# 2. Spatial Merge -------------------------------------------------------------
# Find the lat/lon of the tehsil name in the survey data. Using those coordinates,
# grab the tehsil name from the hdx shapefile.

##### Determine ADMs without a match
unmatched_ADMs_survey <- unique(survey_uc_df$tehsil_name_hdx)[!(unique(survey_uc_df$tehsil_name_hdx) %in% uc_sdf$TEHSIL)]
unmatched_ADMs_shp <- unique(uc_sdf$TEHSIL)[!(unique(uc_sdf$TEHSIL) %in% survey_uc_df$tehsil_name_hdx)]

#### Grab Coordinates
survey_uc_df$tehsil_lat <- NA
survey_uc_df$tehsil_lon <- NA

survey_uc_df$tehsil_lat[survey_uc_df$tehsil_name_original == "18-hazari"] <- 31.167072 
survey_uc_df$tehsil_lon[survey_uc_df$tehsil_name_original == "18-hazari"] <- 72.089819

survey_uc_df$tehsil_lat[survey_uc_df$tehsil_name_original %in% c("allam iqbal town","allam town",  "allama iqbal town", "allama iqbal twon")] <- 31.511111
survey_uc_df$tehsil_lon[survey_uc_df$tehsil_name_original %in% c("allam iqbal town","allam town", "allama iqbal town", "allama iqbal twon")] <- 74.283889

survey_uc_df$tehsil_lat[survey_uc_df$tehsil_name_original == "aziz bhatti town"] <- 32.116667 
survey_uc_df$tehsil_lon[survey_uc_df$tehsil_name_original == "aziz bhatti town"] <- 72.65

survey_uc_df$tehsil_lat[survey_uc_df$tehsil_name_original == "aroop town"] <- 32.220518
survey_uc_df$tehsil_lon[survey_uc_df$tehsil_name_original == "aroop town"] <- 74.219534

survey_uc_df$tehsil_lat[survey_uc_df$tehsil_name_original == "data gb town"] <- 31.566667 # this is a union council in here: https://en.wikipedia.org/wiki/Qila_Gujar_Singh
survey_uc_df$tehsil_lon[survey_uc_df$tehsil_name_original == "data gb town"] <- 74.333333

survey_uc_df$tehsil_lat[survey_uc_df$tehsil_name_original == "d.g khan (tribal area)"] <- 30.033056 
survey_uc_df$tehsil_lon[survey_uc_df$tehsil_name_original == "d.g khan (tribal area)"] <- 70.64

survey_uc_df$tehsil_lat[survey_uc_df$tehsil_name_original == "dina"] <- 33.028333 
survey_uc_df$tehsil_lon[survey_uc_df$tehsil_name_original == "dina"] <- 73.601111

survey_uc_df$tehsil_lat[survey_uc_df$tehsil_name_original == "faisalabad"] <- 31.416667
survey_uc_df$tehsil_lon[survey_uc_df$tehsil_name_original == "faisalabad"] <- 73.091111

survey_uc_df$tehsil_lat[survey_uc_df$tehsil_name_original == "gujar khan"] <- 33.253 
survey_uc_df$tehsil_lon[survey_uc_df$tehsil_name_original == "gujar khan"] <- 73.304

survey_uc_df$tehsil_lat[survey_uc_df$tehsil_name_original == "gujranwala"] <- 32.156667 
survey_uc_df$tehsil_lon[survey_uc_df$tehsil_name_original == "gujranwala"] <- 74.19

survey_uc_df$tehsil_lat[survey_uc_df$tehsil_name_original == "hazro"] <- 33.90927 
survey_uc_df$tehsil_lon[survey_uc_df$tehsil_name_original == "hazro"] <- 72.492

survey_uc_df$tehsil_lat[survey_uc_df$tehsil_name_original == "kahuta"] <- 33.583333 
survey_uc_df$tehsil_lon[survey_uc_df$tehsil_name_original == "kahuta"] <- 73.383333

survey_uc_df$tehsil_lat[survey_uc_df$tehsil_name_original == "kallar kahar"] <- 32.783333 
survey_uc_df$tehsil_lon[survey_uc_df$tehsil_name_original == "kallar kahar"] <- 72.7

survey_uc_df$tehsil_lat[survey_uc_df$tehsil_name_original == "kallar sayaddan"] <- 33.414444 
survey_uc_df$tehsil_lon[survey_uc_df$tehsil_name_original == "kallar sayaddan"] <- 73.378611

survey_uc_df$tehsil_lat[survey_uc_df$tehsil_name_original == "khiali shah pur town"] <- 32.121 
survey_uc_df$tehsil_lon[survey_uc_df$tehsil_name_original == "khiali shah pur town"] <- 74.173

survey_uc_df$tehsil_lat[survey_uc_df$tehsil_name_original == "kot momin"] <- 32.188333 
survey_uc_df$tehsil_lon[survey_uc_df$tehsil_name_original == "kot momin"] <- 73.028611

survey_uc_df$tehsil_lat[survey_uc_df$tehsil_name_original == "kot radha kishen"] <- 31.1725
survey_uc_df$tehsil_lon[survey_uc_df$tehsil_name_original == "kot radha kishen"] <- 74.099722

survey_uc_df$tehsil_lat[survey_uc_df$tehsil_name_original == "kotli sattian"] <- 33.807593
survey_uc_df$tehsil_lon[survey_uc_df$tehsil_name_original == "kotli sattian"] <- 73.525482

survey_uc_df$tehsil_lat[survey_uc_df$tehsil_name_original == "lahore cantt"] <- 31.516667
survey_uc_df$tehsil_lon[survey_uc_df$tehsil_name_original == "lahore cantt"] <- 74.383333

survey_uc_df$tehsil_lat[survey_uc_df$tehsil_name_original == "malakwal"] <- 32.553056
survey_uc_df$tehsil_lon[survey_uc_df$tehsil_name_original == "malakwal"] <- 73.206667

survey_uc_df$tehsil_lat[survey_uc_df$tehsil_name_original == "muridke"] <- 31.802
survey_uc_df$tehsil_lon[survey_uc_df$tehsil_name_original == "muridke"] <- 74.255

survey_uc_df$tehsil_lat[survey_uc_df$tehsil_name_original == "murree"] <- 33.904167
survey_uc_df$tehsil_lon[survey_uc_df$tehsil_name_original == "murree"] <- 73.390278

survey_uc_df$tehsil_lat[survey_uc_df$tehsil_name_original == "nandi pur town"] <- 32.250117
survey_uc_df$tehsil_lon[survey_uc_df$tehsil_name_original == "nandi pur town"] <- 74.267271

survey_uc_df$tehsil_lat[survey_uc_df$tehsil_name_original == "nashter town"] <- 31.478565 # maybe double check
survey_uc_df$tehsil_lon[survey_uc_df$tehsil_name_original == "nashter town"] <- 74.352312

survey_uc_df$tehsil_lat[survey_uc_df$tehsil_name_original == "qila dedar singh town"] <- 32.8
survey_uc_df$tehsil_lon[survey_uc_df$tehsil_name_original == "qila dedar singh town"] <- 74.1

survey_uc_df$tehsil_lat[survey_uc_df$tehsil_name_original == "rajanpur (tribal area)"] <- 29.104167
survey_uc_df$tehsil_lon[survey_uc_df$tehsil_name_original == "rajanpur (tribal area)"] <- 70.324722

survey_uc_df$tehsil_lat[survey_uc_df$tehsil_name_original == "ravi town"] <- 31.616667
survey_uc_df$tehsil_lon[survey_uc_df$tehsil_name_original == "ravi town"] <- 74.3

survey_uc_df$tehsil_lat[survey_uc_df$tehsil_name_original == "rawalpindi"] <- 33.6
survey_uc_df$tehsil_lon[survey_uc_df$tehsil_name_original == "rawalpindi"] <- 73.033333

survey_uc_df$tehsil_lat[survey_uc_df$tehsil_name_original == "samanabad town"] <- 31.536198
survey_uc_df$tehsil_lon[survey_uc_df$tehsil_name_original == "samanabad town"] <- 74.299013

survey_uc_df$tehsil_lat[survey_uc_df$tehsil_name_original == "sambrial"] <- 32.16
survey_uc_df$tehsil_lon[survey_uc_df$tehsil_name_original == "sambrial"] <- 74.4

survey_uc_df$tehsil_lat[survey_uc_df$tehsil_name_original == "sangla hill"] <- 31.713333
survey_uc_df$tehsil_lon[survey_uc_df$tehsil_name_original == "sangla hill"] <- 73.374444

survey_uc_df$tehsil_lat[survey_uc_df$tehsil_name_original == "shah kot"] <- 31.573333
survey_uc_df$tehsil_lon[survey_uc_df$tehsil_name_original == "shah kot"] <- 73.480556

survey_uc_df$tehsil_lat[survey_uc_df$tehsil_name_original == "shaker garh"] <- 32.262778
survey_uc_df$tehsil_lon[survey_uc_df$tehsil_name_original == "shaker garh"] <- 75.158333

survey_uc_df$tehsil_lat[survey_uc_df$tehsil_name_original == "shalimar town"] <- 31.585833 
survey_uc_df$tehsil_lon[survey_uc_df$tehsil_name_original == "shalimar town"] <- 74.381944

survey_uc_df$tehsil_lat[survey_uc_df$tehsil_name_original == "sharak pur"] <- 31.463333
survey_uc_df$tehsil_lon[survey_uc_df$tehsil_name_original == "sharak pur"] <- 74.1

survey_uc_df$tehsil_lat[survey_uc_df$tehsil_name_original == "taxila"] <- 33.745833
survey_uc_df$tehsil_lon[survey_uc_df$tehsil_name_original == "taxila"] <- 72.7875

survey_uc_df$tehsil_lat[survey_uc_df$tehsil_name_original == "wahga town"] <- 31.604722
survey_uc_df$tehsil_lon[survey_uc_df$tehsil_name_original == "wahga town"] <- 74.573056

survey_uc_df$tehsil_lat[survey_uc_df$tehsil_name_original == "zafarwal"] <- 32.21 
survey_uc_df$tehsil_lon[survey_uc_df$tehsil_name_original == "zafarwal"] <- 74.54

##### Extract Names of HDX Tehsil from Coordinates
# Spatail Points Dataframe of Tehsil Lat/Lon
survey_uc_df_tehsil_latlon <- subset(survey_uc_df, select=c(tehsil_name_original, tehsil_lat, tehsil_lon)) %>% unique
survey_uc_df_tehsil_latlon <- survey_uc_df_tehsil_latlon[!is.na(survey_uc_df_tehsil_latlon$tehsil_lat),]
coordinates(survey_uc_df_tehsil_latlon) <- ~tehsil_lon+tehsil_lat
crs(survey_uc_df_tehsil_latlon) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

# Extract Names from Tehsil Lat/Lon
survey_uc_df_tehsil_latlon_OVER_uc_sdf <- over(survey_uc_df_tehsil_latlon, uc_sdf)
survey_uc_df_tehsil_latlon$tehsil_name_hdx <- survey_uc_df_tehsil_latlon_OVER_uc_sdf$TEHSIL 

# Add Names to Survey File
for(i in 1:nrow(survey_uc_df_tehsil_latlon)){
  survey_uc_df$tehsil_name_hdx[survey_uc_df$tehsil_name_original == survey_uc_df_tehsil_latlon$tehsil_name_original[i]] <- survey_uc_df_tehsil_latlon$tehsil_name_hdx[i]
}

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
#####                       *** Union Councils ***                        #####
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

# 1. Clean Names ---------------------------------------------------------------
# When abbreviating number, shapefile uses "no." while survey data uses "no"
survey_uc_df$unioncouncil_name_hdx <- gsub(" no "," no. ", survey_uc_df$unioncouncil_name_original)

# Remove extra white space ("word1    word2" -> "word1 word2")
survey_uc_df$unioncouncil_name_hdx <- gsub("\\s+"," ", survey_uc_df$unioncouncil_name_original)

# 2. Manually match ------------------------------------------------------------
survey_uc_df$unioncouncil_name_hdx[survey_uc_df$unioncouncil_name_original == "khuda bakhsh mehar"] <- "khuda bux mehar"
survey_uc_df$unioncouncil_name_hdx[survey_uc_df$unioncouncil_name_original == "chak no. 89 fateh"] <- "chak no 89 fateh"

# 3. Levenstein distance match -------------------------------------------------
#### Define Function
determine_best_two_local_matches <- function(uc_survey_name_original, 
                                             tehsil_var_name_survey,
                                             district_var_name_survey,
                                             uc_sdf,
                                             uc_var_other_source,
                                             tehsil_var_name_other_source,
                                             district_var_name_other_source){
  # DESCRIPTION: Finds best two matches and levenstein distance of the matches that 
  # are within the same tehsil & district. It is assummed that tehsils and districts
  # names exactly match between the survey data and tehsils and districts.
  # PARAMETERS:
    # uc_survey_name_original: String variable with name of UC
    # uc_sdf: Dataframe or spatial dataframe of other data source
    # uc_var_other_source: variable name containing union councils from the 
                         # other data source (eg, other shapefile)
    # tehsil_var_other_source: variable name containing tehsils from the 
                             # other data source (eg, other shapefile)
    # district_var_other_source: variable name containing districts from the 
                               # other data source (eg, other shapefile)

  # Only implement if name is unique
  if(sum(survey_uc_df$unioncouncil_name_original == uc_survey_name_original) == 1){
    
    # Grab tehsil and district names where union council is
    survey_tehsil_i <- survey_uc_df[[tehsil_var_name_survey]][survey_uc_df$unioncouncil_name_original == uc_survey_name_original]
    survey_district_i <- survey_uc_df[[district_var_name_survey]][survey_uc_df$unioncouncil_name_original == uc_survey_name_original]
    
    #### Restirct list of UCs from Other Source to Consider
    # UCs must be in same district/tehsil
    gis_ucs_in_districttehsil <- uc_sdf[[uc_var_other_source]][uc_sdf[[district_var_name_other_source]] == survey_district_i & 
                                                               uc_sdf[[tehsil_var_name_other_source]] == survey_tehsil_i]
    
    # First letters must be the same
    gis_ucs_in_districttehsil <- gis_ucs_in_districttehsil[substr(gis_ucs_in_districttehsil,1,1) %in% substr(uc_survey_name_original,1,1)]
    
    #### Determine Matches
    # Determine Best Match
    gis_bestmatch <- gis_ucs_in_districttehsil[amatch(uc_survey_name_original, gis_ucs_in_districttehsil, maxDist=20)]
    levenstein_distance <- adist(uc_survey_name_original, gis_bestmatch) %>% as.numeric
    
    # Determine Second Best Match
    gis_ucs_in_districttehsil_minusbest <- gis_ucs_in_districttehsil[gis_ucs_in_districttehsil != gis_bestmatch]
    
    gis_secondmatch <- gis_ucs_in_districttehsil_minusbest[amatch(uc_survey_name_original, gis_ucs_in_districttehsil_minusbest, maxDist=20)]
    levenstein_distance_2nd <- adist(uc_survey_name_original, gis_secondmatch) %>% as.numeric
    
  } else{
    gis_bestmatch <- NA
    levenstein_distance <- NA
    
    gis_secondmatch <- NA
    levenstein_distance_2nd <- NA
  }
  
  df_out <- data.frame(uc_survey_name_original = uc_survey_name_original,
                       gis_bestmatch = gis_bestmatch,
                       levenstein_distance = levenstein_distance,
                       gis_secondmatch = gis_secondmatch,
                       levenstein_distance_2nd = levenstein_distance_2nd)
  
  return(df_out)
}

# 3.1 Match with HDX Data ------------------------------------------------------
# 1. Create dataframe of uc names without a match. 
# 2. Determine first and second best match & levenstein distances. We have 
#    already matched names of tehsils and districts; conseqeuntly, only look
#    at name matches that are within the same tehsil and district.
# 3. Accept first best match under certain circumstances (eg, first best match
#    has a levenstein distance of 1 and second best has a levenstein distance of 10)
# 4. Merge accepted matches back into dataset.

#### Implement Function with HDX data
uc_survey_nomatch_hdx <- survey_uc_df$unioncouncil_name_hdx[!survey_uc_df$unioncouncil_name_hdx %in% uc_sdf$UC]
uc_leven_matches_hdx <- lapply(uc_survey_nomatch_hdx, determine_best_two_local_matches,
                               "tehsil_name_hdx", "district_name_hdx",
                               uc_sdf, "UC", "TEHSIL", "DISTRICT") %>% bind_rows
uc_leven_matches_hdx <- uc_leven_matches_hdx[!is.na(uc_leven_matches_hdx$levenstein_distance),]

### Determine which matches to use
uc_leven_matches_hdx$use_match <- FALSE

### LEVEN DIST 1
# If best match has levenstein distance of 1 and no second best match
uc_leven_matches_hdx$use_match[uc_leven_matches_hdx$levenstein_distance == 1 & is.na(uc_leven_matches_hdx$levenstein_distance_2nd)] <- TRUE

# If best match has levenstein distance of 1 and second match has dist >= 3
uc_leven_matches_hdx$use_match[uc_leven_matches_hdx$levenstein_distance == 1 & uc_leven_matches_hdx$levenstein_distance_2nd %in% 3:100] <- TRUE

### LEVEN DIST 2
# If best match has levenstein distance of 2 and no second best match
uc_leven_matches_hdx$use_match[uc_leven_matches_hdx$levenstein_distance == 2 & is.na(uc_leven_matches_hdx$levenstein_distance_2nd)] <- TRUE

# If best match has levenstein distance of 2 and second best match >= 5 AND "^chak " not in UC name
uc_leven_matches_hdx$use_match[uc_leven_matches_hdx$levenstein_distance == 2 & uc_leven_matches_hdx$levenstein_distance_2nd %in% 5:100 & !grepl("^chak ", uc_leven_matches_hdx$unioncouncil_name_original)] <- TRUE

### LEVEN DIST 3
# If best match has levenstein distance of 3 and no second best match AND "^chak " not in UC name
uc_leven_matches_hdx$use_match[uc_leven_matches_hdx$levenstein_distance == 3 & is.na(uc_leven_matches_hdx$levenstein_distance_2nd) & !grepl("^chak ", uc_leven_matches_hdx$unioncouncil_name_original)] <- TRUE

# If best match has levenstin distance of 3 and second best match has dist >= 9 AND "^chack " not in UC name
uc_leven_matches_hdx$use_match[uc_leven_matches_hdx$levenstein_distance == 3 & uc_leven_matches_hdx$levenstein_distance_2nd %in% 9:100 & !grepl("^chak ", uc_leven_matches_hdx$unioncouncil_name_original)] <- TRUE

### LEVEN DIST 4
# If best match has levenstein distance of 4 and no second best match and "^chack " not in UC name and UC name has at least 6 characters
uc_leven_matches_hdx$use_match[uc_leven_matches_hdx$levenstein_distance == 4 & is.na(uc_leven_matches_hdx$levenstein_distance_2nd) & !grepl("^chak ", uc_leven_matches_hdx$unioncouncil_name_original) & nchar(uc_leven_matches_hdx$unioncouncil_name_original) >= 6] <- TRUE

# If best match has levenstein distance of 4 and second best match has distance >= 10 and "^chack " not in UC name and UC name has at least 6 characters
uc_leven_matches_hdx$use_match[uc_leven_matches_hdx$levenstein_distance == 4 & uc_leven_matches_hdx$levenstein_distance_2nd %in% 10:100 & !grepl("^chak ", uc_leven_matches_hdx$unioncouncil_name_original) & nchar(uc_leven_matches_hdx$unioncouncil_name_original) >= 6] <- TRUE

### LEVEN DIST 5
uc_leven_matches_hdx$use_match[uc_leven_matches_hdx$levenstein_distance == 5 & is.na(uc_leven_matches_hdx$levenstein_distance_2nd) & !grepl("^chak ", uc_leven_matches_hdx$unioncouncil_name_original) & !(uc_leven_matches_hdx$unioncouncil_name_original %in% c("bindyal","abbas pura","jamil abad","son miani","shah wali"))] <- TRUE
uc_leven_matches_hdx$use_match[uc_leven_matches_hdx$levenstein_distance == 5 & uc_leven_matches_hdx$levenstein_distance_2nd %in% 7:100 & !grepl("^chak ", uc_leven_matches_hdx$unioncouncil_name_original) & !(uc_leven_matches_hdx$unioncouncil_name_original %in% c("ladhike","kathyala virkan","parel","saboor"))] <- TRUE

### LEVEN DIST 6
uc_leven_matches_hdx$use_match[uc_leven_matches_hdx$levenstein_distance == 6 & is.na(uc_leven_matches_hdx$levenstein_distance_2nd) & !grepl("^chak ", uc_leven_matches_hdx$unioncouncil_name_original) & !(uc_leven_matches_hdx$unioncouncil_name_original %in% c("bhattian","harsa sheikh","farooqabad","dalowal","amarpura"))] <- TRUE
uc_leven_matches_hdx$use_match[uc_leven_matches_hdx$levenstein_distance == 6 & uc_leven_matches_hdx$levenstein_distance_2nd %in% 10:100 & !grepl("^chak ", uc_leven_matches_hdx$unioncouncil_name_original)] <- TRUE

### LEVEN DIST 7
uc_leven_matches_hdx$use_match[uc_leven_matches_hdx$levenstein_distance == 7 & uc_leven_matches_hdx$levenstein_distance_2nd %in% 8:9 & !grepl("^chak ", uc_leven_matches_hdx$unioncouncil_name_original) & !(uc_leven_matches_hdx$unioncouncil_name_original %in% c("bajra gharhi","bhon fazbla","dianat pura","gullu wali","hardo saharan","maroof","nawan shehr","nawan shehr","phoklian","wanjo wali","hafiz wala","kotha kalan","mahmood abad","roshan bhila","sanjwal cantt"))] <- TRUE
uc_leven_matches_hdx$use_match[uc_leven_matches_hdx$levenstein_distance == 7 & uc_leven_matches_hdx$levenstein_distance_2nd %in% 10:100 & !grepl("^chak ", uc_leven_matches_hdx$unioncouncil_name_original) & !(uc_leven_matches_hdx$unioncouncil_name_original %in% c("khanke mor"))] <- TRUE
uc_leven_matches_hdx$use_match[uc_leven_matches_hdx$levenstein_distance == 7 & is.na(uc_leven_matches_hdx$levenstein_distance_2nd) & !grepl("^chak ", uc_leven_matches_hdx$unioncouncil_name_original) & !(uc_leven_matches_hdx$unioncouncil_name_original %in% c("urban 6", "urban 5","urban 7","urban 1","hali","new multan","abid abad","rehmatabad","parial","ranial"))] <- TRUE

# Sample code to check matches
#candidate_match_df <- uc_leven_matches_hdx[uc_leven_matches_hdx$levenstein_distance == 4 & uc_leven_matches_hdx$levenstein_distance_2nd %in% 10:100 & !grepl("^chak ", uc_leven_matches_hdx$unioncouncil_name_original),]
#uc_leven_matches_hdx$use_match[uc_leven_matches_hdx$levenstein_distance == 4 & uc_leven_matches_hdx$levenstein_distance_2nd %in% 10:100 & !grepl("^chak ", uc_leven_matches_hdx$unioncouncil_name_original)] <- TRUE

### Manaully Accept Certain Ones
uc_leven_matches_hdx$use_match[grepl("^okara [[:digit:]]", uc_leven_matches_hdx$unioncouncil_name_original)] <- TRUE
uc_leven_matches_hdx$use_match[grepl("^attock [[:digit:]]", uc_leven_matches_hdx$unioncouncil_name_original)] <- TRUE
uc_leven_matches_hdx$use_match[grepl("^gujrat [[:digit:]]", uc_leven_matches_hdx$unioncouncil_name_original)] <- TRUE
uc_leven_matches_hdx$use_match[uc_leven_matches_hdx$unioncouncil_name_original %in% "mad pir wala"] <- TRUE
uc_leven_matches_hdx$use_match[uc_leven_matches_hdx$unioncouncil_name_original %in% "ghar maharaja"] <- TRUE
uc_leven_matches_hdx$use_match[uc_leven_matches_hdx$unioncouncil_name_original %in% "shah pur uc"] <- TRUE
uc_leven_matches_hdx$use_match[uc_leven_matches_hdx$unioncouncil_name_original %in% "korianwali"] <- TRUE
uc_leven_matches_hdx$use_match[uc_leven_matches_hdx$unioncouncil_name_original %in% "barasajwar khan"] <- TRUE
uc_leven_matches_hdx$use_match[uc_leven_matches_hdx$unioncouncil_name_original %in% "kamra cantt"] <- TRUE
uc_leven_matches_hdx$use_match[uc_leven_matches_hdx$unioncouncil_name_original %in% "kamra kalan"] <- TRUE
uc_leven_matches_hdx$use_match[uc_leven_matches_hdx$unioncouncil_name_original %in% "donga akoka"] <- TRUE
uc_leven_matches_hdx$use_match[uc_leven_matches_hdx$unioncouncil_name_original %in% "musa bhota"] <- TRUE
uc_leven_matches_hdx$use_match[uc_leven_matches_hdx$unioncouncil_name_original %in% "rojhanwali"] <- TRUE
uc_leven_matches_hdx$use_match[uc_leven_matches_hdx$unioncouncil_name_original %in% "chak no. 12bc"] <- TRUE
uc_leven_matches_hdx$use_match[uc_leven_matches_hdx$unioncouncil_name_original %in% "chak no. 24b.c"] <- TRUE
uc_leven_matches_hdx$use_match[uc_leven_matches_hdx$unioncouncil_name_original %in% "chak no. 37/b.c"] <- TRUE
uc_leven_matches_hdx$use_match[uc_leven_matches_hdx$unioncouncil_name_original %in% "chak no. 4b.c"] <- TRUE
uc_leven_matches_hdx$use_match[uc_leven_matches_hdx$unioncouncil_name_original %in% "jamal chand"] <- TRUE
uc_leven_matches_hdx$use_match[uc_leven_matches_hdx$unioncouncil_name_original %in% "chak no. 60/61 ml"] <- TRUE
uc_leven_matches_hdx$use_match[uc_leven_matches_hdx$unioncouncil_name_original %in% "ali pur syedan"] <- TRUE
uc_leven_matches_hdx$use_match[uc_leven_matches_hdx$unioncouncil_name_original %in% "bhalwal -iv"] <- TRUE
uc_leven_matches_hdx$use_match[uc_leven_matches_hdx$unioncouncil_name_original %in% "bhera -i"] <- TRUE
uc_leven_matches_hdx$use_match[uc_leven_matches_hdx$unioncouncil_name_original %in% "157 10 r"] <- TRUE
uc_leven_matches_hdx$use_match[uc_leven_matches_hdx$unioncouncil_name_original %in% "abas pur"] <- TRUE
uc_leven_matches_hdx$use_match[uc_leven_matches_hdx$unioncouncil_name_original %in% "aman gharh"] <- TRUE
uc_leven_matches_hdx$use_match[uc_leven_matches_hdx$unioncouncil_name_original %in% "androon bhaati gate uc"] <- TRUE
uc_leven_matches_hdx$use_match[uc_leven_matches_hdx$unioncouncil_name_original %in% "androon bhatti gate uc"] <- TRUE
uc_leven_matches_hdx$use_match[uc_leven_matches_hdx$unioncouncil_name_original %in% "androon dehli gate uc"] <- TRUE
uc_leven_matches_hdx$use_match[uc_leven_matches_hdx$unioncouncil_name_original %in% "arif wala eb/147"] <- TRUE
uc_leven_matches_hdx$use_match[uc_leven_matches_hdx$unioncouncil_name_original %in% "badorta"] <- TRUE
uc_leven_matches_hdx$use_match[uc_leven_matches_hdx$unioncouncil_name_original %in% "bali wala"] <- TRUE
uc_leven_matches_hdx$use_match[uc_leven_matches_hdx$unioncouncil_name_original %in% "baloksar"] <- TRUE
uc_leven_matches_hdx$use_match[uc_leven_matches_hdx$unioncouncil_name_original %in% "bankey cheema"] <- TRUE
uc_leven_matches_hdx$use_match[uc_leven_matches_hdx$unioncouncil_name_original %in% "bariat (chak bawa)"] <- TRUE
uc_leven_matches_hdx$use_match[uc_leven_matches_hdx$unioncouncil_name_original %in% "barj atari"] <- TRUE
uc_leven_matches_hdx$use_match[uc_leven_matches_hdx$unioncouncil_name_original %in% "basir pur 1"] <- TRUE
uc_leven_matches_hdx$use_match[uc_leven_matches_hdx$unioncouncil_name_original %in% "basir pur 2"] <- TRUE
#uc_leven_matches_CHECK <- uc_leven_matches[uc_leven_matches$use_match == FALSE,]

#### Merge matches back into dataset
uc_leven_matches_hdx <- uc_leven_matches_hdx[uc_leven_matches_hdx$use_match == TRUE,]

for(i in 1:nrow(uc_leven_matches_hdx)){
  survey_uc_df$unioncouncil_name_hdx[survey_uc_df$unioncouncil_name_original == uc_leven_matches_hdx$uc_survey_name_original[i]] <- uc_leven_matches_hdx$gis_bestmatch[i]
}

# Export =======================================================================
survey_uc_df$unioncouncil_name_hdx[!(survey_uc_df$unioncouncil_name_hdx %in% uc_sdf$UC)] <- NA

survey_uc_df <- survey_uc_df %>%
  dplyr::select(id, district_name_hdx, tehsil_name_hdx, unioncouncil_name_hdx)

saveRDS(survey_uc_df, file.path(final_data_file_path, "UC with NSER Data", "individual_files", "nsersurvey_matched_hdx_shapefile.Rds"))
