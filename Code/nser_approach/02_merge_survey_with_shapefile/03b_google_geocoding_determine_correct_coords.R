# Merge Survey Data with Pakistan Boundaries (Union Councils)

# Load Data ====================================================================
#### Union Council Survey Data
survey_uc_df <- readRDS(file.path(final_data_file_path, "UC with NSER Data", "individual_files", "nsersurvey_matched_hdx_shapefile.Rds"))

survey_uc_df$full_address <- paste(survey_uc_df$unioncouncil_name_original,
                                   survey_uc_df$tehsil_name_original,
                                   survey_uc_df$district_name_original,
                                   "punjab",
                                   "pakistan",
                                   sep=", ") %>% 
  tolower %>% 
  str_squish()

#### Google API Data
google_geocoder_output <- read.csv(file.path(project_file_path, "Data", "FinalData", "UC Google Geocoder Output", "uc_google_geocoder_output.csv"))
names(google_geocoder_output)[-10] <- paste0("google_", names(google_geocoder_output)[-10]) 

survey_uc_df <- merge(survey_uc_df, google_geocoder_output, by.x="full_address", by.y="address_input")

survey_uc_df <- survey_uc_df[!is.na(survey_uc_df$google_lat),]
survey_uc_df$accept_google_coords <- FALSE

#### HDX Union Council Shapefile 
uc_sdf <- readOGR(dsn=file.path(project_file_path, "Data", "RawData", "Pakistan Boundaries", "HDX"),
                  layer="Union_Council")

uc_sdf@data$PROVINCE <- uc_sdf$PROVINCE %>% as.character %>% tolower 
uc_sdf@data$DISTRICT <- uc_sdf$DISTRICT %>% as.character %>% tolower
uc_sdf@data$TEHSIL <- uc_sdf$TEHSIL %>% as.character %>% tolower
uc_sdf@data$UC <- uc_sdf$UC %>% as.character %>% tolower

# Prep Data ====================================================================

# Extract UC, Tehsil, District Names from Google Output ------------------------
split_address_into_parts <- function(address){
  address_split <- str_split(address, ",")[[1]] %>% str_squish()
  
  if(length(address_split) >= 4){
    adm0 <- address_split[length(address_split)]
    adm1 <- address_split[length(address_split)-1]
    adm2 <- address_split[length(address_split)-2]
    #uc_name <- paste(address_split[1:(length(address_split)-3)], collapse=", ")
    uc_name <- address_split[1]
    
    df_out <- data.frame(googlelatlon_country = adm0,
                         googlelatlon_province = adm1,
                         googlelatlon_district = adm2,
                         googlelatlon_uc = uc_name)
  } else{
    df_out <- data.frame(googlelatlon_country = NA,
                         googlelatlon_province = NA,
                         googlelatlon_district = NA,
                         googlelatlon_uc = NA)
  }
  
  return(df_out)
}

google_latlon_adm <- lapply(survey_uc_df$google_address, split_address_into_parts) %>% bind_rows
survey_uc_df <- cbind(survey_uc_df, google_latlon_adm)

# Extract ADM Names from HDX Shapefile -----------------------------------------
# Subset to obs with coordinates and define as spatial dataframe
survey_uc_df_googlelatlon <- survey_uc_df
survey_uc_df_googlelatlon <- subset(survey_uc_df_googlelatlon, select=c(full_address, google_lat, google_lon))
coordinates(survey_uc_df_googlelatlon) <- ~google_lon+google_lat
crs(survey_uc_df_googlelatlon) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

# Extract ADM names from HDX shapefile 
googlelatlon_OVER_uc_sdf <- over(survey_uc_df_googlelatlon, uc_sdf)
googlelatlon_OVER_uc_sdf <- googlelatlon_OVER_uc_sdf %>%
  dplyr::select(PROVINCE, DISTRICT, TEHSIL, UC) %>%
  dplyr::rename(province_google_latlon_from_shp = PROVINCE) %>%
  dplyr::rename(district_google_latlon_from_shp = DISTRICT) %>%
  dplyr::rename(tehsil_google_latlon_from_shp = TEHSIL) %>%
  dplyr::rename(uc_google_latlon_from_shp = UC)
googlelatlon_OVER_uc_sdf$full_address <- survey_uc_df_googlelatlon$full_address

survey_uc_df <- merge(survey_uc_df, googlelatlon_OVER_uc_sdf, by="full_address", all.x=T)

# Match Name with Google Geocoder ----------------------------------------------

# TODO [CODE HERE]

# Match Google Coords/HDX Shapefile Location -----------------------------------
# Using Google Coordinates and HDX shapefile, extract name of uc, tehsil & district
# from HDX that coordinates fall in. Accept coordinates if these names are NSER 
# original names.

#### Google output must be in same country, province & district, tehsil as UC
survey_uc_df_matchshp <- survey_uc_df

survey_uc_df_matchshp <- survey_uc_df_matchshp[!is.na(survey_uc_df_matchshp$district_google_latlon_from_shp),]
survey_uc_df_matchshp <- survey_uc_df_matchshp["punjab" == survey_uc_df_matchshp$province_google_latlon_from_shp,]
survey_uc_df_matchshp <- survey_uc_df_matchshp[survey_uc_df_matchshp$district_name_hdx == survey_uc_df_matchshp$district_google_latlon_from_shp,]
survey_uc_df_matchshp <- survey_uc_df_matchshp[survey_uc_df_matchshp$tehsil_name_hdx == survey_uc_df_matchshp$tehsil_google_latlon_from_shp,]

#### Exact Match
survey_uc_df_matchshp$accept_google_coords[survey_uc_df_matchshp$unioncouncil_name_original == survey_uc_df_matchshp$uc_google_latlon_from_shp] <- TRUE

#### Fuzzy Match
# Levenstein Distance
survey_uc_df_matchshp$survey_googlelatlon_uc_levenstein <- lapply(1:nrow(survey_uc_df_matchshp), function(i){
  out <- adist(survey_uc_df_matchshp$unioncouncil_name_original[i], survey_uc_df_matchshp$uc_google_latlon_from_shp[i]) %>% as.numeric
  return(out)
}) %>% unlist

# Accept all with levensteing distance of 1
survey_uc_df_matchshp$accept_google_coords[survey_uc_df_matchshp$survey_googlelatlon_uc_levenstein %in% 1] <- TRUE

# Accept all with levensteing distance of 2
survey_uc_df_matchshp$accept_google_coords[survey_uc_df_matchshp$survey_googlelatlon_uc_levenstein %in% 2] <- TRUE

# Accept all with levensteing distance of 3 [except certian words]
words_dont_accept <- c("chak no. 199/e.b", "chak no. 223/e.b", "chak no. 225/e.b", "chak no. 257/e.b", "chak no. 287/e.b", "chak no. 495/e.b", "chak no. 521/e.b", "chawa", "jalala")
survey_uc_df_matchshp$accept_google_coords[survey_uc_df_matchshp$survey_googlelatlon_uc_levenstein %in% 3 & !(survey_uc_df_matchshp$unioncouncil_name %in% words_dont_accept)] <- TRUE

#### Merge back into original dataframe
survey_uc_df_matchshp <- survey_uc_df_matchshp[survey_uc_df_matchshp$accept_google_coords,]
survey_uc_df$accept_google_coords[survey_uc_df$id %in% survey_uc_df_matchshp$id] <- TRUE

# Export Data ==================================================================
survey_uc_df <- survey_uc_df[survey_uc_df$accept_google_coords,]
survey_uc_df <- survey_uc_df %>%
  dplyr::select(id, google_lat, google_lon)

saveRDS(survey_uc_df, file.path(final_data_file_path, "UC with NSER Data", "individual_files", "nsersurvey_matched_google_coords.Rds"))
