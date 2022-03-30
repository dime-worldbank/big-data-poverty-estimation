# Extract road density and distance to nearest road for each survey location
# and road type

# Functions ====================================================================
load_prep_osm_roads <- function(country_code, 
                                osm_dir_df, 
                                data_split_into_subsets,
                                subset_id = NULL){
  # DESCRIPTION: Load and Prep OSM Road Data
  # ARGS
  # -- country_code: Two letter country code
  # -- osm_dir_df: Dataframe with country codes and OSM directories
  # -- cc_epsg: Projection
  # -- data_split_into_subsets: If the dataset is split into segments
  # -- subset_id: If the data is split into subsets, what is the subset_id?
  
  ### A. Define directory
  osm_country_dir <- osm_dir_df$osm_dirs[osm_dir_df$country_code %in% country_code]
  
  ### B. Load data
  print("Load OSM data")
  if(data_split_into_subsets){
    osm_df <- readRDS(file.path(osm_dir, "FinalData", osm_country_dir, 
                                paste0("gis_osm_roads_free_1_SUBSET_",subset_id,".Rds")))
  } else{
    osm_df <- readRDS(file.path(osm_dir, "FinalData", osm_country_dir, 
                                "gis_osm_roads_free_1.Rds"))
  }
  
  ### C. Cleanup
  print("Cleanup OSM data")
  osm_df$fclass <- osm_df$fclass %>% as.character()
  osm_df$fclass[grepl("trunk", osm_df$fclass)]     <- "trunk"
  osm_df$fclass[grepl("motorway", osm_df$fclass)]  <- "motorway"
  osm_df$fclass[grepl("primary", osm_df$fclass)]   <- "primary"
  osm_df$fclass[grepl("secondary", osm_df$fclass)] <- "secondary"
  osm_df$fclass[grepl("tertiary", osm_df$fclass)]  <- "tertiary"
  osm_df$fclass[grepl("track", osm_df$fclass)]     <- "track"
  
  ### D. Remove certain classes
  print("Subset OSM data")
  osm_df <- osm_df[!(osm_df$fclass %in% c("steps",
                                          "unknown",
                                          "bridleway",
                                          "cycleway")),]
  
  print(table(osm_df$fclass))
  
  ### E. Project
  if(F){
    print("Project OSM data")
    osm_df <- osm_df %>% spTransform(CRS(cc_epsg))
  }
  
  ### F. To SF
  print("OSM data to SF")
  osm_sf <- osm_df %>% st_as_sf()
  
  return(osm_sf)
}

# 1. Load survey data ----------------------------------------------------------
survey_df <- readRDS(file.path(data_dir, SURVEY_NAME, "FinalData", "Individual Datasets", "survey_socioeconomic.Rds"))

if(SURVEY_NAME %in% "DHS"){
  survey_df <- survey_df %>%
    dplyr::filter(most_recent_survey %in% T)
}

survey_df <- survey_df %>%
  dplyr::select(uid, country_code, year, latitude, longitude, GID_2) %>%
  dplyr::filter(!is.na(latitude)) %>%
  dplyr::mutate(uid = uid %>% as.character)

if(SURVEY_NAME %in% "OPM"){
  survey_df <- survey_df %>%
    distinct(uid, .keep_all = T)
}

coordinates(survey_df) <- ~longitude+latitude
crs(survey_df) <- CRS("+init=epsg:4326")
survey_sf <- survey_df %>% st_as_sf()

country_codes_all <- survey_df$country_code %>% unique()

# 2. Load country_code to OSM dir data -----------------------------------------
# Make dataset that has [country_code] and [osm_root_name] (root name of OSM dir)

## Survey Details
survey_details_df <- read_xlsx(file.path(cntry_dtls_dir, "survey_details.xlsx"))
survey_proj_df <- survey_details_df %>%
  dplyr::select(country_code, epsg_projection)

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

# 3. Extract density -----------------------------------------------------------
country_codes_all <- unique(survey_sf$country_code)
country_codes_all <- country_codes_all[country_codes_all != "GY"]

for(country_code in country_codes_all){
  
  #### Load road files 
  # Sometimes we split road files into subsets. So either load the subsets or 
  # just the full road file
  osm_country_dir <- osm_dir_df$osm_dirs[osm_dir_df$country_code %in% country_code] 
  
  osm_country_road_files <- file.path(osm_dir, "FinalData", osm_country_dir) %>%
    list.files() %>%
    str_subset("gis_osm_roads_free")
  
  # If contains subsets, only use the subsets
  if(TRUE %in% grepl("SUBSET", osm_country_road_files)){
    osm_country_road_files <- osm_country_road_files[grepl("SUBSET", osm_country_road_files)]
    
    subset_id <- osm_country_road_files %>%
      str_replace_all("gis_osm_roads_free_1_SUBSET_", "") %>%
      str_replace_all(".Rds", "")
    data_split_into_subsets <- T
  } else{
    subset_id <- "1"
    data_split_into_subsets <- F
  }
  
  #### Loop through road files (either full road file or all the subsets)
  for(subset_id_i in subset_id){
    
    OUT_PATH <- file.path(data_dir, SURVEY_NAME, "FinalData", 
                          "Individual Datasets", "osm", "roads_distance",
                          paste0("osm_roaddistance_", country_code, "_subset_",subset_id_i,".Rds"))
    print(OUT_PATH)
    
    if(REPLACE_IF_EXTRACTED | !file.exists(OUT_PATH)){
      
      #### Load OSM
      osm_sf <- load_prep_osm_roads(country_code,
                                    osm_dir_df, 
                                    data_split_into_subsets,
                                    subset_id_i)
      
      #### Load Survey
      survey_sf_i <- survey_sf[survey_sf$country_code %in% country_code,]
      
      if(country_code == "IA") survey_sf_i <- survey_sf_i[survey_sf_i$GID_2 %in% subset_id_i,]
      
      #### Extract distance
      osm_distance_country_i <- lapply(unique(osm_sf$fclass), function(fclass_i){
        print(fclass_i)
        
        osm_sf_i <- osm_sf[osm_sf$fclass %in% fclass_i,]
        
        osm_nearest_id <- st_nearest_feature_chunks(survey_sf_i, osm_sf_i, 1000)
        osm_nearest_sf_i <- osm_sf_i[osm_nearest_id,]
        
        dist <- st_distance(survey_sf_i, osm_nearest_sf_i, by_element = T)
        
        out_df <- data.frame(uid = survey_sf_i$uid,
                             dist = as.numeric(dist))
        names(out_df)[names(out_df) %in% "dist"] <- paste0("osm_dist_", fclass_i)
        
        return(out_df)
      }) %>%
        reduce(left_join, by = "uid")
      
      #### Export
      saveRDS(osm_distance_country_i, OUT_PATH)
    }
  }
}



