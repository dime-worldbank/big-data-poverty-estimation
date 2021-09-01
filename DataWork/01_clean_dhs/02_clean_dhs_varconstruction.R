# Clean DHS Data
# Variable Construction

set.seed(42)

## PARMETERS
N_OBS_IN_TFR <- 200 # max number of observations in a tfrecord

over_nearest <- function(sp_i, gadm_i){
  # For each sp_i, takes the nearest polygon from gadm_i. Returns data frame
  # ob sp_i with gadm data merged in
  
  # Add uids in same order as rows
  gadm_i$gadm_uid <- 1:nrow(gadm_i)
  
  # gadm uid, where intersect
  sp_i$gadm_uid <- over(sp_i, gadm_i)$gadm_uid
  
  # determine gadm uid of nearest (if doesn't intersect)
  sp_nonintersect_i <- sp_i[is.na(sp_i$gadm_uid),]
  
  if(nrow(sp_nonintersect_i) > 0){
    gadm_uid_nonintersect <- lapply(1:nrow(sp_nonintersect_i), function(i){
      print(paste(i, "/", nrow(sp_nonintersect_i)))
      gDistance(sp_nonintersect_i[i,],
                gadm_i,
                byid = T) %>% which.min()
    }) %>% unlist()
    
    sp_i$gadm_uid[is.na(sp_i$gadm_uid)] <- gadm_uid_nonintersect
  }
  
  # Add GADM data
  df_i <- as.data.frame(sp_i)
  df_i <- merge(df_i, gadm_i@data, by = "gadm_uid")
  
  return(df_i)
}


# Load Data --------------------------------------------------------------------
dhs_all_df <- readRDS(file.path(dhs_dir, "FinalData", "Individual Datasets", "survey_socioeconomic_hhlevel.Rds"))

## Subset - needs coordinates
dhs_all_df <- dhs_all_df %>%
  dplyr::filter(!is.na(latitude))

## Remove if issues extracting daytime data 
dhs_all_df <- dhs_all_df[!(dhs_all_df$uid %in% c("IA201400180079",
                                                 "IA201400180052")),]

# Add variable most recent and subset to most recent ---------------------------

dhs_all_df <- dhs_all_df %>%
  dplyr::group_by(country_code) %>%
  dplyr::mutate(latest_survey_country = max(year)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(most_recent_survey = latest_survey_country == year) %>%
  dplyr::select(-latest_survey_country)

dhs_all_df <- dhs_all_df %>%
  dplyr::filter(most_recent_survey %in% T)

# Asset Index ------------------------------------------------------------------
#### Cleanup variables

# Number of sleeping rooms; if 0, say 1
dhs_all_df$n_rooms_sleeping[dhs_all_df$n_rooms_sleeping %in% 0] <- 1

# Floor material
dhs_all_df$floor_material_not_earth <- dhs_all_df$floor_material %>% str_detect("earth")
dhs_all_df$floor_material_not_earth <- dhs_all_df$floor_material_not_earth %in% FALSE

# Water source
dhs_all_df$water_source_piped_dwelling <- as.numeric(dhs_all_df$water_source %in% "piped into dwelling")

dhs_all_df <- dhs_all_df %>%
  mutate_at(vars(contains("has")), tidyr::replace_na, 0) %>%
  mutate(n_sleeping_rooms_pp = n_hh_members / n_rooms_sleeping) %>%
  dplyr::filter(!is.na(n_rooms_sleeping))

pca_1 <- dhs_all_df %>% 
  dplyr::select(contains("has"), 
                n_sleeping_rooms_pp,
                floor_material_not_earth,
                water_source_piped_dwelling) %>%
  prcomp(scale = T)

dhs_all_df$asset_pca_1 <- pca_1$x[,1]

# Aggregate --------------------------------------------------------------------
dhs_all_df_coll <- dhs_all_df %>%
  group_by(uid, country_code, country_year, urban_rural, year, most_recent_survey,
           latitude, longitude) %>%
  summarise_if(is.numeric, mean, na.rm=T) %>%
  ungroup()

# Within Country Folds ---------------------------------------------------------
dhs_gadm_cw <- bind_rows(
  data.frame(code_dhs = "BD", code_gadm = "BGD"),
  data.frame(code_dhs = "IA", code_gadm = "IND"),
  data.frame(code_dhs = "KH", code_gadm = "KHM"),
  data.frame(code_dhs = "KY", code_gadm = "KGZ"),
  data.frame(code_dhs = "MM", code_gadm = "MMR"),
  data.frame(code_dhs = "NP", code_gadm = "NPL"),
  data.frame(code_dhs = "PH", code_gadm = "PHL"),
  data.frame(code_dhs = "PK", code_gadm = "PAK"),
  data.frame(code_dhs = "TJ", code_gadm = "TJK"),
  data.frame(code_dhs = "TL", code_gadm = "TLS")
) %>%
  mutate(code_dhs = code_dhs %>% as.character,
         code_gadm = code_gadm %>% as.character)

dhs_all_df_coll <- map_df(c("BD", "IA", "KH", "KY", "MM", "NP",
                            "PH", "PK", "TJ", "TL"), function(cc_dhs){
                              print(cc_dhs)
                              
                              cc_gadm <- dhs_gadm_cw$code_gadm[dhs_gadm_cw$code_dhs %in% cc_dhs]
                              
                              df_i <- dhs_all_df_coll[dhs_all_df_coll$country_code %in% cc_dhs,]
                              gadm_i <- readRDS(file.path(data_dir, "GADM", "RawData", paste0("gadm36_",cc_gadm,"_2_sp.rds")))
                              
                              gadm_i[,c("GID_0", "GID_1", "GID_2",
                                        "NAME_0", "NAME_1", "NAME_2")]
                              
                              # Random fold
                              within_country_fold <- rep_len(1:5, length.out = nrow(gadm_i)) %>% sample()
                              gadm_i$within_country_fold <- paste0(cc_dhs, "_", within_country_fold)
                              
                              sp_i <- df_i
                              coordinates(sp_i) <- ~longitude+latitude
                              crs(sp_i) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
                              
                              df_out <- over_nearest(sp_i, gadm_i)
                              
                              return(df_out)
                            }) 

# Make Folder Name -------------------------------------------------------------
# Folder name for storing individual tfrecords. Google drive complains when
# too many files in an individual folder

#dhs_all_df_coll$tf_folder_name <- dhs_all_df_coll$uid %>% substring(1,11)
#dhs_all_df_coll$tf_folder_name[is.na(dhs_all_df_coll$tf_folder_name)] <- "other"
#dhs_all_df_coll <- dhs_all_df_coll[!is.na(dhs_all_df_coll$uid),]

dhs_all_df_coll$tfrecord_name <- ""

for(fold_name in unique(dhs_all_df_coll$within_country_fold)){
  N_in_fold <- sum(dhs_all_df_coll$within_country_fold %in% fold_name)
  
  tfrecord_id <- rep(1:10000, times=1, each=N_OBS_IN_TFR)[1:N_in_fold]
  tfrecord_id <- sample(tfrecord_id)
  
  tfrecord_name <- paste0(fold_name, "_", tfrecord_id, ".tfrecord")
  
  dhs_all_df_coll$tfrecord_name[dhs_all_df_coll$within_country_fold %in% fold_name] <- tfrecord_name
}

# Export -----------------------------------------------------------------------
## All
saveRDS(dhs_all_df_coll, file.path(dhs_dir, "FinalData", "Individual Datasets", "survey_socioeconomic.Rds"))
write.csv(dhs_all_df_coll, file.path(dhs_dir, "FinalData", "Individual Datasets", "survey_socioeconomic.csv"), row.names = F)

saveRDS(dhs_all_df_coll, file.path(gdrive_file_path, "Data", "DHS", "FinalData", "Individual Datasets", "survey_socioeconomic.Rds"))
write.csv(dhs_all_df_coll, file.path(gdrive_file_path, "Data", "DHS", "FinalData", "Individual Datasets", "survey_socioeconomic.csv"), row.names = F)

saveRDS(dhs_all_df_coll, file.path(secure_file_path, "Data", "DHS",  "FinalData - PII", "survey_socioeconomic_geo.Rds"))
write.csv(dhs_all_df_coll, file.path(secure_file_path, "Data", "DHS",  "FinalData - PII", "survey_socioeconomic_geo.csv"), row.names = F)

## Geo Only
df_geoonly <- dhs_all_df_coll %>%
  dplyr::select(uid, latitude, longitude, urban_rural, most_recent_survey, country_code, year)

saveRDS(df_geoonly, file.path(secure_file_path, "Data", "DHS",  "FinalData - PII", "GPS_uid_crosswalk.Rds"))
write.csv(df_geoonly, file.path(secure_file_path, "Data", "DHS",  "FinalData - PII", "GPS_uid_crosswalk.csv"), row.names = F)




head(df)

dfa <- df %>%
  dplyr::filter(uid %in% c("IA201400180079",
                           "IA201400180052",
                           "IA201400180112",
                           "IA201400180081",
                           "IA201400180011",
                           "IA201400180048",
                           "IA201400180058",
                           "IA201400180028",
                           "IA201400180072",
                           "IA201400180047",
                           "IA201400180012",
                           "IA201400180040",
                           "IA201400180055",
                           "IA201400180140",
                           "IA201400180030",
                           "IA201400180104",
                           "IA201400180123",
                           "IA201400180062",
                           "IA201400180080",
                           "IA201400180050",
                           "IA201400180116"))

leaflet() %>%
  addTiles() %>%
  addCircles(data = df[df$NAME_2 %in% "Lakshadweep",])

dfa$gadm_uid


(df$GID_2 %in% "IND.18.1_1") %>% table()
