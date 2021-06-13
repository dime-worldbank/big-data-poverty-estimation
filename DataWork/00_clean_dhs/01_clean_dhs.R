# Clean DHS Data

# Clean DHS survey data. Create Household Level 
# dataframe with relevant socioeconomic variables.

# Load Data --------------------------------------------------------------------
geo_0607 <- readOGR(file.path(dhs_dir, "RawData", "PK_2006_07", "PKGE51FL", 'PKGE51FL.shp'))
hh_0607 <- read_dta(file.path(dhs_dir, "RawData", "PK_2006_07", "PKHR52DT", 'pkhr52fl.dta'))

geo_1718 <- readOGR(file.path(dhs_dir, "RawData", "PK_2017_18", "PKGE71FL", 'PKGE71FL.shp'))
hh_1718 <- read_dta(file.path(dhs_dir, "RawData", "PK_2017_18", "PKHR71DT", 'PKHR71FL.DTA'))

# Clean Geo Data ---------------------------------------------------------------
# Clean geospatial data

clean_geo <- function(df){
  df <- df@data %>%
    dplyr::filter(SOURCE != "MIS") %>% # Missing lat/lon
    dplyr::rename(cluster_id = DHSCLUST,
                  latitude = LATNUM,
                  longitude = LONGNUM,
                  urban_rural = URBAN_RURA,
                  year = DHSYEAR,
                  country_code = DHSCC) %>%
    dplyr::select(cluster_id, latitude, longitude, urban_rural, year, country_code)
  
  return(df)
}

geo_0607 <- clean_geo(geo_0607)
geo_1718 <- clean_geo(geo_1718)

# Clean HH Data ----------------------------------------------------------------
# Clean HH data and collapse to cluster level

clean_hh <- function(df){
  df <- df %>%
    dplyr::rename(cluster_id = hv001,
                  wealth_index = hv270,
                  wealth_index_score = hv271) %>%
    dplyr::select(cluster_id, wealth_index, wealth_index_score) %>%
    group_by(cluster_id) %>%
    dplyr::summarise_all(mean, na.rm=T)
  
  return(df)
}

hh_0607 <- clean_hh(hh_0607)
hh_1718 <- clean_hh(hh_1718)

# Merge and Clean --------------------------------------------------------------
# Merge and final cleaning

merge_clean <- function(hh_df, geo_df){
  
  df_out <- geo_df %>%
    left_join(hh_df, by = "cluster_id") %>%
    dplyr::mutate(uid = paste0(country_code, year, cluster_id))
  
  return(df_out)
}

df_0607 <- merge_clean(hh_0607, geo_0607)
df_1718 <- merge_clean(hh_1718, geo_1718)

# Append -----------------------------------------------------------------------
df <- bind_rows(df_0607, df_1718)

df <- df %>%
  mutate_if(is.factor, as.character)

# Add Geo Data -----------------------------------------------------------------
grid <- readRDS(file.path(project_file_path, "Data", "Country Grid", "FinalData", "pak_region_grid_200km.Rds"))
grid@data <- grid@data %>%
  dplyr::rename(tile_id = id)

df_sdf <- df
coordinates(df_sdf) <- ~longitude+latitude
crs(df_sdf) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

df$tile_id <- over(df_sdf, grid)$tile_id

# Export -----------------------------------------------------------------------
## All
saveRDS(df, file.path(dhs_dir, "FinalData", "Individual Datasets", "survey_socioeconomic.Rds"))
write.csv(df, file.path(dhs_dir, "FinalData", "Individual Datasets", "survey_socioeconomic.csv"), row.names = F)

saveRDS(df, file.path(secure_file_path, "Data", "DHS",  "FinalData - PII", "survey_socioeconomic_geo.Rds"))
write.csv(df, file.path(secure_file_path, "Data", "DHS",  "FinalData - PII", "survey_socioeconomic_geo.csv"), row.names = F)

## Geo Only
df_geoonly <- df %>%
  dplyr::select(uid, latitude, longitude, urban_rural, tile_id)

saveRDS(df_geoonly, file.path(secure_file_path, "Data", "DHS",  "FinalData - PII", "GPS_uid_crosswalk.Rds"))
write.csv(df_geoonly, file.path(secure_file_path, "Data", "DHS",  "FinalData - PII", "GPS_uid_crosswalk.csv"), row.names = F)


