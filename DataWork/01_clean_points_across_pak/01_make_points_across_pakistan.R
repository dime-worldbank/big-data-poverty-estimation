# Make points across Pakistan
# Use Facebook RWI locations

# Load data --------------------------------------------------------------------
fb_df <- readRDS(file.path(fb_rwi_dir, "FinalData", "fb_rwi.Rds"))

fb_df <- fb_df %>%
  dplyr::filter(iso2s %in% "PK") %>% 
  dplyr::mutate(year = 2020,
                country_code = "PK",
                country_name = "Pakistan",
                most_recent_survey = T) 

# Export -----------------------------------------------------------------------
saveRDS(fb_df,
        file.path(data_dir, "PAK_POINTS", "FinalData", "Individual Datasets", "survey_socioeconomic.Rds"))

# a <- fb_df %>%
#   arrange(latitude, longitude) %>%
#   head(1500)
# 
# coordinates(a) <- ~longitude+latitude
# crs(a) <- CRS("+init=epsg:4326")
# 
# ab <- gBuffer(a, byid = T, width = 1.5/111.12)
# 
# leaflet() %>%
#   addTiles() %>%
#   addPolygons(data = ab)


