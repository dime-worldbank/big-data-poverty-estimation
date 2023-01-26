# Merge Facebook RWI with Survey Data, using nearest neighbor

set.seed(42)

# Load data --------------------------------------------------------------------
fb_df <- readRDS(file.path(fb_rwi_dir, "FinalData", "fb_rwi.Rds"))
survey_df <- readRDS(file.path(data_dir, SURVEY_NAME, "FinalData", "Individual Datasets", 
                               "survey_socioeconomic.Rds"))

survey_df <- survey_df[survey_df$iso2 %in% fb_df$iso2s,]

# Nearest Neighbor Within Country ----------------------------------------------
survey_df <- map_df(unique(survey_df$iso2), function(iso2){
  
  ## Grab data in specific country
  survey_df_iso <- survey_df[survey_df$iso2 %in% iso2,]
  fb_df_iso <- fb_df[fb_df$iso2 %in% iso2,]
  
  leaflet() %>%
    addTiles() %>%
    addCircles(data = survey_df_iso) %>%
    addCircles(data = fb_df_iso, color = "red")
  
  ## Add closest Facebook ID to Survey Data
  closest <- nn2(fb_df_iso[,c("latitude", "longitude")] %>% as.matrix(), 
                 survey_df_iso[,c("latitude", "longitude")] %>% as.matrix(), 
                 k = 1)
  closest <- sapply(closest, cbind) %>% as_tibble
  
  survey_df_iso$fb_id <- closest$nn.idx
  
  ## Merge FB data to survey
  fb_df_iso <- fb_df_iso %>%
    dplyr::mutate(fb_id = 1:n()) %>%
    dplyr::select(fb_id, uid, rwi, error, latitude, longitude) %>%
    dplyr::rename(fb_quadkey = uid,
                  fb_rwi = rwi,
                  fb_rwi_error = error,
                  fb_rwi_latitude = latitude,
                  fb_rwi_longitude = longitude)
  
  survey_df_iso <- left_join(survey_df_iso, fb_df_iso, by = "fb_id")
  
  return(survey_df_iso)
})

#### Determine distance of Facebook RWI point and survey point
survey_df$dist_fbrwi_km <- distGeo(p1 = as.matrix(survey_df[,c("longitude","latitude")]),
                                   p2 = as.matrix(survey_df[,c("fb_rwi_longitude","fb_rwi_latitude")])) / 1000

#### If distance is too far, remove
survey_df$fb_rwi[survey_df$dist_fbrwi_km >= 2] <- NA
survey_df$fb_rwi_error[survey_df$dist_fbrwi_km >= 2] <- NA

# Export -----------------------------------------------------------------------
survey_df <- survey_df %>%
  dplyr::select(uid, fb_rwi, fb_rwi_error)

saveRDS(survey_df, file.path(data_dir, SURVEY_NAME, "FinalData", "Individual Datasets", 
                             "fb_relative_wealth.Rds"))




survey_df_i <- survey_df[survey_df$dist_fbrwi_km >= 5,]
survey_df_i$country_code %>% table()

head(fb_df)
fb_df$iso3s %>% unique() %>% length()
unique(survey_df$iso2)[!(unique(survey_df$iso2) %in% fb_df$iso2s)]

survey_df$iso2 %>% table()

survey_df$country_name[survey_df$country_code %in% "MB"] %>% unique()


