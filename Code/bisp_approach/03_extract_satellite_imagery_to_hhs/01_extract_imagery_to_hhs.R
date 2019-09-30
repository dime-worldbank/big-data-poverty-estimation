# Extract Satellite Data to HH Coordinates

# DESCRIPTION
# Buffer HH coordinates and extract average satellite values.

BUFFER_KM <- 1

# Load and Prep HH Data --------------------------------------------------------------------
hh_cluster <- read.csv(file.path(rawdata_file_path, "Landsat", "bisp_households", "bisp_hh_cluster_id_crosswalk.csv"))
hh_coords <- read_dta(file.path(bisp_geocodes_file_path, "GPS_uid_crosswalk.dta"))

#### Merge Cluster ID
hh_coords$uid_numeric <- as.numeric(hh_coords$uid)
hh_cluster <- hh_cluster %>%
  dplyr::rename(uid_numeric = uid)

hh_coords <- merge(hh_coords, hh_cluster, by="uid_numeric", all.x=T, all.y=F)

#### Convert Coordinates
get_lat_long <- function(number) {
  deg <- floor(number / 100)
  min <- floor(number - (100 * deg))
  sec <- 100 * (number - (100 * deg) - min)
  degree <- deg + (min / 60) + (sec / 3600)
  
  return(degree)
}

hh_coords <- hh_coords %>%
  mutate(lat = get_lat_long(GPSN),
         long = get_lat_long(GPSE))

#### Remove NAs and High/Wrong Lat Lon
hh_coords <- hh_coords[!is.na(hh_coords$GPSN),]
hh_coords <- hh_coords[hh_coords$lat < 100,]
hh_coords <- hh_coords[hh_coords$long < 100,]

hh_coords <- hh_coords[!(hh_coords$cluster_id %in% c("1334", "1576", "5158", "5282")),]

#### Keep Select Variables
hh_coords <- hh_coords %>%
  dplyr::select(uid, cluster_id, lat, long)

# Convert to Spatial Buffer ----------------------------------------------------
hh_coords_sp <- hh_coords
coordinates(hh_coords_sp) <- ~long+lat
hh_coords_sp <- gBuffer(hh_coords_sp, width=BUFFER_KM/111.12, byid=T)
crs(hh_coords_sp) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

# Extract VIIRS ----------------------------------------------------------------
for(year in 2012:2018){
  print(year)
  viirs <- raster(file.path(rawdata_file_path, "VIIRS", "VIIRS Annual", paste0("pak_viirs_median_",year,".tif")))
  hh_coords_sp[[paste0("viirs_", year)]] <- velox(viirs)$extract(sp = hh_coords_sp, fun=mean)
}

# Extract DMSP-OLS ----------------------------------------------------------------
for(year in 1992:2013){
  print(year)
  dmspols <- raster(file.path(rawdata_file_path, "DMSPOLS", paste0("pak_dmspols_",year,".tif")))
  hh_coords_sp[[paste0("dmspols_", year)]] <- velox(dmspols)$extract(sp = hh_coords_sp, fun=mean)
}

# Extract 2011 Landsat ---------------------------------------------------------
l7_2011_df <- lapply(rev(unique(hh_coords_sp$cluster_id)), function(cluster_i){
  print(cluster_i)
  
  hh_coords_cluster_i <- hh_coords_sp[hh_coords_sp$cluster_id == cluster_i,]
  
  landsat <- stack(file.path(rawdata_file_path, "Landsat", "bisp_households", "2011", "stacked", paste0(cluster_i, ".tif")))
  landsat_bands_df <- velox(landsat)$extract(sp=hh_coords_cluster_i, fun=mean) %>% as.data.frame
  names(landsat_bands_df) <- paste0("l7_2011_",1:7)
  landsat_bands_df$uid <- hh_coords_cluster_i$uid
  
  return(landsat_bands_df)
}) %>% bind_rows

hh_coords_sp@data <- merge(hh_coords_sp@data, l7_2011_df, by="uid")

# Extract 2013 Landsat ---------------------------------------------------------
l7_2013_df <- lapply(rev(unique(hh_coords_sp$cluster_id)), function(cluster_i){
  print(cluster_i)
  
  hh_coords_cluster_i <- hh_coords_sp[hh_coords_sp$cluster_id == cluster_i,]
  
  landsat <- stack(file.path(rawdata_file_path, "Landsat", "bisp_households", "2013", "stacked", paste0(cluster_i, ".tif")))
  landsat_bands_df <- velox(landsat)$extract(sp=hh_coords_cluster_i, fun=mean) %>% as.data.frame
  names(landsat_bands_df) <- paste0("l7_2013_",1:7)
  landsat_bands_df$uid <- hh_coords_cluster_i$uid
  
  return(landsat_bands_df)
}) %>% bind_rows

hh_coords_sp@data <- merge(hh_coords_sp@data, l7_2013_df, by="uid")

# Export -----------------------------------------------------------------------
hh_coords_df <- hh_coords_sp@data %>%
  dplyr::select(-cluster_id)
saveRDS(hh_coords_df, file.path(final_data_file_path, "BISP", paste0("bisp_satellite_data_buffer_",BUFFER_KM,"km.Rds")))







