# Extract number of POIs and distance to nearest POIs for each survey
# location and type of POI

# Define functions -------------------------------------------------------------
load_osm_poi <- function(country_code, osm_dir_df){
  
  ### A. Define directory
  osm_country_dir <- osm_dir_df$osm_dirs[osm_dir_df$country_code %in% country_code]
  
  ### B. Load data
  osm1_df <- readRDS(file.path(osm_dir, "FinalData", osm_country_dir, "gis_osm_pois_free_1.Rds"))
  osm2_df <- readRDS(file.path(osm_dir, "FinalData", osm_country_dir, "gis_osm_pois_a_free_1.Rds"))
  
  ### C. Prep data and spatially define
  osm1_coords_df <- osm1_df %>% 
    coordinates() %>% 
    as.data.frame() %>%
    dplyr::rename(longitude = 1,
                  latitude = 2) %>%
    bind_cols(osm1_df@data)
  
  osm2_coords_df <- osm2_df %>% 
    coordinates() %>% 
    as.data.frame() %>%
    dplyr::rename(longitude = 1,
                  latitude = 2) %>%
    bind_cols(osm2_df@data)
  
  osm_df <- bind_rows(osm1_coords_df,
                      osm2_coords_df) %>%
    distinct(osm_id, .keep_all = T)
  
  coordinates(osm_df) <- ~longitude+latitude
  crs(osm_df) <- CRS("+init=epsg:4326")
  
  osm_df$one <- 1
  
  return(osm_df)
}

extract_n_poi <- function(buffer_m, country_code, survey_df, osm_dir_df){
  
  # 1. Prep survey data --------------------------------------------------------
  # Subset and buffer
  survey_df <- survey_df[survey_df$country_code %in% country_code,]
  
  survey_df <- survey_df %>%
    dplyr::select(uid, latitude, longitude)
  
  coordinates(survey_df) <- ~longitude+latitude
  crs(survey_df) <- "+init=epsg:4326"
  
  survey_buff_df <- geo.buffer_chunks(survey_df, r = buffer_m, chunk_size = 100)
  survey_buff_df$one <- 1
  
  # 2. Load and prep OSM data --------------------------------------------------
  osm_df <- load_osm_poi(country_code, osm_dir_df)
  
  # 3. N Poi Nearby ------------------------------------------------------------
  for(class_i in unique(osm_df$fclass[!is.na(osm_df$fclass)])){
    print(class_i)
    
    osm_df_classi <- osm_df[osm_df$fclass %in% class_i,]
    
    class_df <- over(osm_df_classi, survey_buff_df)$uid %>%
      as.character() %>%
      as.data.frame() %>%
      dplyr::rename(uid = ".") %>%
      dplyr::filter(!is.na(uid)) %>%
      group_by(uid) %>%
      dplyr::summarise(N = n()) %>%
      ungroup()
    
    names(class_df)[names(class_df) %in% "N"] <- paste0("osm_n_poi_", class_i)
    
    survey_df <- merge(survey_df, class_df, by = "uid", all.x=T)
    survey_df@data[paste0("osm_n_poi_", class_i)][is.na(survey_df@data[paste0("osm_n_poi_", class_i)])] <- 0
  }
  
  # 4. Cleanup -----------------------------------------------------------------
  survey_df <- survey_df@data
  
  survey_df <- survey_df %>%
    dplyr::rename_at(vars(-uid), ~ paste0(., "_",buffer_m,"m_buff"))
  
  return(survey_df)
}

extract_dist_poi <- function(country_code, survey_df, osm_dir_df){
  
  # 1. Prep survey data --------------------------------------------------------
  # Subset and buffer
  survey_df <- survey_df[survey_df$country_code %in% country_code,]
  
  survey_df <- survey_df %>%
    dplyr::select(uid, latitude, longitude)
  
  coordinates(survey_df) <- ~longitude+latitude
  crs(survey_df) <- "+init=epsg:4326"
  
  survey_sf <- survey_df %>% st_as_sf()
  
  # 2. Load and prep OSM data --------------------------------------------------
  osm_df <- load_osm_poi(country_code, osm_dir_df)
  
  # 3. Distance to Class --------------------------------------------------------
  for(class_i in unique(osm_df$fclass[!is.na(osm_df$fclass)])){
    
    osm_df_classi <- osm_df[osm_df$fclass %in% class_i,]
    
    print(paste0(class_i, " - ", nrow(osm_df_classi)))
    
    ## Grab one observation; replace geometry in next step
    osm_df_classi_agg <- osm_df_classi[1,] %>% st_as_sf()
    
    osm_df_classi_combine <- osm_df_classi %>%
      st_as_sf() %>%
      st_combine()
    
    osm_df_classi_agg$geometry <- st_geometry(osm_df_classi_combine)
    
    #osm_df_classi_agg <- osm_df_classi %>%
    #  gBuffer_chunks(width = 0.0001, chunk_size = 2000) %>%
    #  raster::aggregate(by = "one") %>%
    #  st_as_sf()
    
    
    if(nrow(osm_df_classi) <= 50000){
      buffer_chunk_n <- 3000
    } else{
      buffer_chunk_n <- 1000
    }
    
    survey_df[[paste0("osm_distmeters_poi_", class_i)]] <- st_distance_chunks(survey_sf, osm_df_classi_agg, buffer_chunk_n)
  } 
  
  return(survey_df@data)
}

# Load survey data -------------------------------------------------------------
survey_df <- readRDS(file.path(data_dir, SURVEY_NAME, "FinalData", "Individual Datasets", "survey_socioeconomic.Rds"))

survey_df <- survey_df %>%
  dplyr::select(uid, country_code, year, latitude, longitude) %>%
  dplyr::filter(!is.na(latitude))

if(SURVEY_NAME %in% "OPM"){
  survey_df <- survey_df %>%
    distinct(uid, .keep_all = T)
}

country_codes_all <- survey_df$country_code %>% unique()
country_codes_all <- country_codes_all[country_codes_all != "GY"]

# Load country_code to OSM dir data --------------------------------------------
# Make dataset that has [country_code] and [osm_root_name] (root name of OSM dir)

## Survey Details
survey_details_df <- read_xlsx(file.path(cntry_dtls_dir, "survey_details.xlsx"))
survey_details_df <- survey_details_df %>%
  dplyr::select(country_code, osm_root_name)

## OSM directories
# If multiple, choose latest
osm_dirs <- list.files(file.path(osm_dir, "FinalData"))

osm_dir_df <- data.frame(osm_dirs = osm_dirs)
osm_dir_df <- osm_dir_df %>%
  dplyr::mutate(osm_root_name = osm_dirs %>%
                  str_replace_all("2.*", "") %>%
                  str_replace_all("1.*", "") %>%
                  str_replace_all("-$", ""),
                osm_dirs = osm_dirs %>% as.character()) %>%
  arrange(desc(osm_dirs)) %>%
  distinct(osm_root_name, .keep_all = T) %>%
  left_join(survey_details_df, by = "osm_root_name") %>%
  dplyr::filter(!is.na(country_code))

# Implement Functions ----------------------------------------------------------
if(SURVEY_NAME %in% "DHS"){
  BUFFERS_TO_USE <- 5000
} else if(SURVEY_NAME %in% "PAK_POINTS"){
  BUFFERS_TO_USE <- 1500
} else{
  BUFFERS_TO_USE <- 5000
}

#### N POI
for(buffer_i in BUFFERS_TO_USE){
  for(country_code_i in country_codes_all){
    print(paste0("N POI: ", country_code_i, " - ", buffer_i, " =============="))
    
    OUT_PATH <- file.path(data_dir, SURVEY_NAME, 
                          "FinalData", "Individual Datasets", "osm", "poi", 
                          paste0("osm_",country_code_i,"_n_poi_",buffer_i,"m_buff.Rds"))
    
    if(!file.exists(OUT_PATH) | REPLACE_IF_EXTRACTED){
      survey_df_i <- extract_n_poi(buffer_i, country_code_i, survey_df, osm_dir_df)
      saveRDS(survey_df_i, OUT_PATH)
    }
  }
}

#### Dist POI
for(country_code_i in country_codes_all){
  print(paste0("DIST POI: ", country_code_i, " =============================="))
  
  OUT_PATH <- file.path(data_dir, SURVEY_NAME, 
                        "FinalData", "Individual Datasets", "osm", "poi", 
                        paste0("osm_",country_code_i,"_dist_poi_buff.Rds"))
  
  if(!file.exists(OUT_PATH) | REPLACE_IF_EXTRACTED){
    survey_df_i <- extract_dist_poi(country_code_i, survey_df, osm_dir_df)
    saveRDS(survey_df_i, OUT_PATH)
  }
}




