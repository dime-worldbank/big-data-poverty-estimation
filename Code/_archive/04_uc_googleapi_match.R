# Match Based on Google API

# Load Data --------------------------------------------------------------------
survey_uc_df <- readRDS(file.path(final_data_file_path,"UC with NSER Data", "individual_files", "02_districttehsil_shpmatch.Rds"))


# Google Geocoder --------------------------------------------------------------
survey_uc_df_googlelatlon <- survey_uc_df
survey_uc_df_googlelatlon <- survey_uc_df_googlelatlon[!is.na(survey_uc_df_googlelatlon$google_lat),]
survey_uc_df_googlelatlon <- subset(survey_uc_df_googlelatlon, select=c(full_address, google_lat, google_lon))
coordinates(survey_uc_df_googlelatlon) <- ~google_lon+google_lat
crs(survey_uc_df_googlelatlon) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

googlelatlon_OVER_uc_sdf <- over(survey_uc_df_googlelatlon, uc_sdf)
googlelatlon_OVER_uc_sdf <- googlelatlon_OVER_uc_sdf %>%
  dplyr::select(PROVINCE, DISTRICT, TEHSIL, UC) %>%
  dplyr::rename(province_google_latlon_from_shp = PROVINCE) %>%
  dplyr::rename(district_google_latlon_from_shp = DISTRICT) %>%
  dplyr::rename(tehsil_google_latlon_from_shp = TEHSIL) %>%
  dplyr::rename(uc_google_latlon_from_shp = UC)
googlelatlon_OVER_uc_sdf$full_address <- survey_uc_df_googlelatlon$full_address

survey_uc_df <- merge(survey_uc_df, googlelatlon_OVER_uc_sdf, by="full_address", all.x=T)

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

#### Determine whether should consider google output
# Google output must be in same country, province & district as UC
survey_uc_df$consider_google_api <- (survey_uc_df$googlelatlon_district %in% survey_uc_df$district_name_gislayer) & (survey_uc_df$googlelatlon_province %in% "punjab") & (survey_uc_df$googlelatlon_country %in% "pakistan") & (!is.na(survey_uc_df$googlelatlon_uc))
survey_uc_df$use_googleapi_output <- FALSE

#### Exact Match
survey_uc_df$use_googleapi_output[survey_uc_df$consider_google_api %in% TRUE] <- survey_uc_df$unioncouncil_name[survey_uc_df$consider_google_api %in% TRUE] %in% survey_uc_df$googlelatlon_uc[survey_uc_df$consider_google_api %in% TRUE]

#### Fuzzy Match
# Levenstein Distance
survey_uc_df$survey_google_uc_levenstein <- lapply(1:nrow(survey_uc_df), function(i){
  out <- adist(survey_uc_df$unioncouncil_name[i], survey_uc_df$googlelatlon_uc[i]) %>% as.numeric
  return(out)
}) %>% unlist

# Accept all with levensteing distance of 1
survey_uc_df$use_googleapi_output[survey_uc_df$consider_google_api == TRUE & survey_uc_df$survey_google_uc_levenstein %in% 1] <- TRUE

# Accept all with levensteing distance of 2
survey_uc_df$use_googleapi_output[survey_uc_df$consider_google_api == TRUE & survey_uc_df$survey_google_uc_levenstein %in% 2] <- TRUE

# Accept all with levensteing distance of 3 [except certian words]
words_dont_accept <- c("chak no. 199/e.b", "chak no. 223/e.b", "chak no. 225/e.b", "chak no. 257/e.b", "chak no. 287/e.b", "chak no. 495/e.b", "chak no. 521/e.b", "chawa", "jalala")
survey_uc_df$use_googleapi_output[survey_uc_df$consider_google_api == TRUE & survey_uc_df$survey_google_uc_levenstein %in% 3 & !(survey_uc_df$unioncouncil_name %in% words_dont_accept)] <- TRUE

# STILL NOT MATCHED
#still_not_matched <- survey_uc_df[!((survey_uc_df$unioncouncil_name %in% uc_sdf$UC) | (survey_uc_df$use_googleapi_output == TRUE)),]
#a <- still_not_matched[still_not_matched$unioncouncil_name %in% "abadi ghkhowal",]
#table((survey_uc_df$unioncouncil_name %in% uc_sdf$UC) | (survey_uc_df$use_googleapi_output == TRUE))

#### Change UC Name Based on Google Output
survey_uc_df$unioncouncil_name[survey_uc_df$use_googleapi_output == TRUE] <- survey_uc_df$uc_google_latlon_from_shp[survey_uc_df$use_googleapi_output == TRUE]

paste(survey_uc_df$district_name, survey_uc_df$tehsil_name, survey_uc_df$unioncouncil_name) %>% length
paste(survey_uc_df$district_name, survey_uc_df$tehsil_name, survey_uc_df$unioncouncil_name) %>% unique %>% length

# Export Data ==================================================================
survey_uc_df$tehsil_name_gislayer <- survey_uc_df$tehsil_name
survey_uc_df$district_name_gislayer <- survey_uc_df$district_name
survey_uc_df$unioncouncil_name_gislayer <- survey_uc_df$unioncouncil_name

survey_uc_df$tehsil_name <- survey_uc_df$tehsil_name_original
survey_uc_df$district_name <- survey_uc_df$district_name_original
survey_uc_df$unioncouncil_name <- survey_uc_df$unioncouncil_name_original

# If no name match, make NA
survey_uc_df$district_name_gislayer[!(survey_uc_df$district_name_gislayer %in% uc_sdf@data$DISTRICT)] <- NA
survey_uc_df$tehsil_name_gislayer[!(survey_uc_df$tehsil_name_gislayer %in% uc_sdf@data$TEHSIL)] <- NA
survey_uc_df$unioncouncil_name_gislayer[!(survey_uc_df$unioncouncil_name_gislayer %in% uc_sdf@data$UC)] <- NA

survey_uc_df$district_name_howfoundmatch[is.na(survey_uc_df$district_name_gislayer)] <- NA
survey_uc_df$tehsil_name_howfoundmatch[is.na(survey_uc_df$tehsil_name_gislayer)] <- NA
survey_uc_df$unioncouncil_name_howfoundmatch[is.na(survey_uc_df$unioncouncil_name_gislayer)] <- NA

# Remove Unneeded Variables
survey_uc_df <- subset(survey_uc_df, select=-c(district_name_original, tehsil_name_original, unioncouncil_name_original, tehsil_lat, tehsil_lon, tehsil_sdf))

# Export CSV -------------------------------------------------------------------
write.csv(survey_uc_df, file.path(project_file_path, "Data", "FinalData", "Pakistan Boundaries", "nser_data_uc.csv"), row.names=F)

# Merge with Shapefile and Export ----------------------------------------------
survey_uc_df$province_name_gislayer <- "punjab"

#### Aggregate Survey Dataset
survey_uc_df_agg <- summaryBy(no_rooms_mean + no_rooms_median + tv_p + aircooler_p + 
                                freezer_p + washingmachine_p + airconditioner_p + heater_p +
                                microwaveoven_p + cookingrange_p + sheep_p + bull_p + cow_p + buffalo_p +
                                goat_p + pmt_score_mean + pmt_score_median + asset_item_index_mean + asset_item_index_median + 
                                asset_animal_index_mean + asset_animal_index_median + households_N + individuals_N + age_mean +
                                age_median ~ 
                                province_name_gislayer + 
                                district_name_gislayer + 
                                tehsil_name_gislayer + 
                                unioncouncil_name_gislayer, data=survey_uc_df, FUN=mean, keep.names=T)

uc_sdf_agg <- raster::aggregate(uc_sdf, by=c("PROVINCE","DISTRICT","TEHSIL","UC"))
uc_sdf_agg <- uc_sdf_agg[uc_sdf_agg$PROVINCE %in% "punjab",]

uc_with_data_sdf <- merge(uc_sdf_agg, survey_uc_df_agg, 
                          by.x=c("PROVINCE","DISTRICT","TEHSIL","UC"),
                          by.y=c("province_name_gislayer", "district_name_gislayer", "tehsil_name_gislayer", "unioncouncil_name_gislayer"), all.x=T)
uc_with_data_sdf <- uc_with_data_sdf[!is.na(uc_with_data_sdf$no_rooms_mean),]

#### Add Unique ID
uc_with_data_sdf$uc_id <- paste0("uc_",1:nrow(uc_with_data_sdf))

#### Export
saveRDS(uc_with_data_sdf, file.path(project_file_path, "Data", "FinalData", "UC with NSER Data", "uc_nser.Rds"))

uc_with_data_sf <- st_as_sf(uc_with_data_sdf)
st_write(uc_with_data_sf, file.path(project_file_path, "Data", "FinalData", "UC with NSER Data", "uc_nser.geojson"),delete_dsn=T)

writeOGR(obj=uc_with_data_sdf,
         dsn=file.path(project_file_path,"Data", "FinalData", "UC with NSER Data"),
         layer="uc_nser",
         driver="ESRI Shapefile",
         overwrite_layer=T)

uc_with_data_sdf_nodata <- subset(uc_with_data_sdf, select=c(uc_id))
writeOGR(obj=uc_with_data_sdf_nodata,
         dsn=file.path(project_file_path,"Data", "FinalData", "UC with NSER Data", "File for GEE Asset Upload"),
         layer="uc_nser",
         driver="ESRI Shapefile",
         overwrite_layer=T)




