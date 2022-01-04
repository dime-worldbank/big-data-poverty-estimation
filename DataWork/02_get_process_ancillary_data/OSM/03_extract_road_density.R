# Extract road density and distance to nearest road for each survey location
# and road type

buffer_m <- BUFFER_OSM

# Functions ====================================================================
load_prep_osm_roads <- function(country_code, 
                                osm_dir_df, 
                                #cc_epsg,
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
  #print("Project OSM data")
  #osm_df <- osm_df %>% spTransform(CRS(cc_epsg))
  
  ### F. To SF
  print("OSM data to SF")
  osm_sf <- osm_df %>% st_as_sf()
  
  return(osm_sf)
}

extract_density_fclass_i <- function(fclass_i, 
                                     osm_sf, 
                                     survey_buff_sf, 
                                     survey_buff_sf_agg,
                                     subset_roads_near_buffer = TRUE){
  # DESCRIPTION: Extract road density for class fclass_i from osm_sf for each 
  # location within survey_buff_sf
  # ARGS:
  # -- fclass_i: Road class (eg, "trunk").
  # -- osm_sf: OSM road network (sf object). Must have "fclass" variable.
  # -- survey_buff_sf: Buffered dataset (sf object). Must have "uid" variable.
  # -- survey_buff_sf_agg: Above dataset, but st_union() applied
  
  print(fclass_i)
  
  #### 1. Subset and prep OSM data
  ## Subset
  osm_sf_i <- osm_sf[osm_sf$fclass %in% fclass_i,]
  
  ## Subset network to roads within buffer distance
  # Reduces size of road network, which makes later steps faster. This step only
  # worthwhile if doing so will really limit the size of the datset. (May not
  # happen in cases where surveys are densely scattered across a country).
  if(subset_roads_near_buffer){
    print("Subseting roads near buffer...")
    osm_sf_i <- st_intersection_chunks(osm_sf_i, survey_buff_sf_agg, 2000)
    nrow(osm_sf_i)
  }
  
  #### 2. Extract density for each observation
  road_density_df <- map_df(1:nrow(survey_buff_sf), function(i){
    if((i %% 50) == 0) print(paste0(i, " - Extracting: ", fclass_i))
    
    survey_buff_sf_i <- survey_buff_sf[i,]
    
    #### Extract roads that intersect with buffer
    intersects_tf <- st_intersects(survey_buff_sf_i, osm_sf_i, sparse = F) %>% as.vector()
    road_in_buffer <- st_intersection(osm_sf_i[intersects_tf,], survey_buff_sf_i)
    
    if(nrow(road_in_buffer) > 0){
      length_out <- road_in_buffer %>% st_length() %>% sum() %>% as.numeric() 
      N_segments_out <- road_in_buffer %>% nrow()
    } else{
      length_out <- 0
      N_segments_out <- 0
    }
    
    out <- data.frame(uid = survey_buff_sf_i$uid,
                      length = length_out,
                      N_segments = N_segments_out)
    return(out)
  })
  
  #### 4. Cleanup dataframe
  road_density_df <- road_density_df %>% 
    rename_at(vars(-uid), ~ paste0(., '_', fclass_i, "_", buffer_m, "m")) %>%
    dplyr::mutate(uid = uid %>% as.character())
  
  return(road_density_df)
}

# 1. Load survey data ----------------------------------------------------------
survey_df <- readRDS(file.path(data_dir, SURVEY_NAME, "FinalData", "Individual Datasets", "survey_socioeconomic.Rds"))

survey_df <- survey_df %>%
  dplyr::select(uid, country_code, year, latitude, longitude, GID_2) %>%
  dplyr::filter(!is.na(latitude)) %>%
  dplyr::mutate(uid = uid %>% as.character)

if(SURVEY_NAME %in% "OPM"){
  survey_df <- survey_df %>%
    distinct(uid, .keep_all = T)
}

country_codes_all <- survey_df$country_code %>% unique()
country_codes_all <- country_codes_all[country_codes_all != "GY"]

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

# 3. Pre-Prep India Data -------------------------------------------------------
# India is broken into subsets, so to avoid re-buffering the survey data
# (which takes a bit of time), just do once

# survey_df_IA <- survey_df[survey_df$country_code %in% "IA",]
# 
# survey_df_IA <- survey_df_IA %>%
#   dplyr::select(uid, latitude, longitude)
# 
# coordinates(survey_df_IA) <- ~longitude+latitude
# crs(survey_df_IA) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 "
# 
# print("Buffering IA suvery")
# survey_buff_df_IA <- geo.buffer_chunks(survey_df_IA, r = buffer_m, chunk_size = 100)
# 
# survey_buff_sf_IA <- survey_buff_df_IA %>% st_as_sf()
#survey_buff_sf_agg_IA <- survey_buff_sf_IA %>% st_union()

# 3. Extract density -----------------------------------------------------------
for(country_code in country_codes_all){ 
  
  #### Country-specifc parameters
  # Lots of India survey locations, so subsetting roads to near a survey location
  # doesn't have as much value added to increase the speed of code.
  if(country_code %in% "IA"){
    subset_roads_near_buffer <- T
  } else{
    subset_roads_near_buffer <- T
  }
  
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
                          "Individual Datasets", "osm", "roads_density",
                          paste0("osm_roaddensity_", buffer_m, "m_", country_code, "_subset_",subset_id_i,".Rds"))
    print(OUT_PATH)
    
    if(REPLACE_IF_EXTRACTED | !file.exists(OUT_PATH)){
      
      #### Grab projection for country i
      #epsg_num <- survey_proj_df$epsg_projection[survey_proj_df$country_code %in% country_code]
      #cc_epsg <- paste0("+init=epsg:",epsg_num)
      
      #### Load OSM
      osm_sf <- load_prep_osm_roads(country_code,
                                    osm_dir_df, 
                                    #cc_epsg = "BLANK",
                                    data_split_into_subsets,
                                    subset_id_i)
      
      #### Prep survey data
      # Subset and buffer
      survey_df_i <- survey_df[survey_df$country_code %in% country_code,]
      
      if(country_code == "IA") survey_df_i <- survey_df_i[survey_df_i$GID_2 %in% subset_id_i,]
      if(nrow(survey_df_i) %in% 0) next
      
      survey_df_i <- survey_df_i %>%
        dplyr::select(uid, latitude, longitude)
      
      coordinates(survey_df_i) <- ~longitude+latitude
      crs(survey_df_i) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 "
      
      print("Buffering suvery")
      survey_buff_df_i <- geo.buffer_chunks(survey_df_i, r = buffer_m, chunk_size = 100)
      #survey_buff_df_i <- survey_buff_df_i %>% spTransform(CRS(cc_epsg))
      
      survey_buff_sf_i <- survey_buff_df_i %>% st_as_sf()
      survey_buff_sf_agg_i <- survey_buff_sf_i %>% st_union()
      
      #### Extract density
      osm_density_country_i <- lapply(unique(osm_sf$fclass),
                                      extract_density_fclass_i,
                                      osm_sf, 
                                      survey_buff_sf_i, 
                                      survey_buff_sf_agg_i,
                                      subset_roads_near_buffer) %>%
        reduce(left_join, by = "uid")
      
      saveRDS(osm_density_country_i, OUT_PATH)
    }
  }
}


