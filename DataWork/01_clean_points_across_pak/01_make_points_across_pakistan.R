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

# Add GADM ---------------------------------------------------------------------
pak_sp <- readRDS(file.path(gadm_dir, "RawData", "gadm36_PAK_2_sp.rds"))

## Spatially define points
fb_sp <- fb_df
coordinates(fb_sp) <- ~longitude+latitude
crs(fb_sp) <- CRS("+init=epsg:4326")

fb_OVER_pak <- sp::over(fb_sp, pak_sp)

fb_sp$NAME_1 <- fb_OVER_pak$NAME_1
fb_sp$NAME_2 <- fb_OVER_pak$NAME_2
fb_sp$GID_1  <- fb_OVER_pak$GID_1
fb_sp$GID_2  <- fb_OVER_pak$GID_2

## Not all points intersect with polygon, so use closest polygon
fb_sp_inter_sp   <- fb_sp[!is.na(fb_sp$NAME_1),]
fb_sp_nointer_sp <- fb_sp[is.na(fb_sp$NAME_1),]

fb_sp_nointer_sp_data <- lapply(1:nrow(fb_sp_nointer_sp), function(i){
  
  fb_sp_nointer_sp_i <- fb_sp_nointer_sp[i,]
  
  min_id_to_gadm <- gDistance(fb_sp_nointer_sp_i, pak_sp, byid = T) %>% 
    as.numeric() %>%
    which.min()
  
  pak_sp_i <- pak_sp[min_id_to_gadm,]
  
  fb_sp_nointer_sp_i$NAME_1 <- pak_sp_i$NAME_1
  fb_sp_nointer_sp_i$NAME_2 <- pak_sp_i$NAME_2
  fb_sp_nointer_sp_i$GID_1  <- pak_sp_i$GID_1
  fb_sp_nointer_sp_i$GID_2  <- pak_sp_i$GID_2
  
  return(fb_sp_nointer_sp_i)
}) %>%
  do.call(what = "rbind")

## Append points that intersect and those that dont (where data added in)
fb_sp <- rbind(fb_sp_inter_sp,
               fb_sp_nointer_sp_data)

fb_df <- as.data.frame(fb_sp)

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


