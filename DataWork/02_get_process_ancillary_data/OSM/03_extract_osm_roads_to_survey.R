# Extract road density and distance to nearest road for each survey location
# and road type

buffer_size_osm_m <- c(5000)
SURVEY_NAME <- "DHS"

RE_EXTRACT_IF_EXISTS <- F

# Functions ====================================================================
load_prep_osm_roads <- function(country_code, osm_dir_df, cc_epsg){
  # DESCRIPTION: Load and Prep OSM Road Data
  # ARGS
  # -- country_code: Two letter country code
  # -- osm_dir_df: Dataframe with country codes and OSM directories
  # -- cc_epsg: Projection
  
  ### A. Define directory
  osm_country_dir <- osm_dir_df$osm_dirs[osm_dir_df$country_code %in% country_code]
  
  ### B. Load data
  osm_df <- readRDS(file.path(osm_dir, "FinalData", osm_country_dir, "gis_osm_roads_free_1.Rds"))
  
  ### C. Cleanup
  osm_df$fclass <- osm_df$fclass %>% as.character()
  osm_df$fclass[grepl("trunk", osm_df$fclass)]     <- "trunk"
  osm_df$fclass[grepl("motorway", osm_df$fclass)]  <- "motorway"
  osm_df$fclass[grepl("primary", osm_df$fclass)]   <- "primary"
  osm_df$fclass[grepl("secondary", osm_df$fclass)] <- "secondary"
  osm_df$fclass[grepl("tertiary", osm_df$fclass)]  <- "tertiary"
  osm_df$fclass[grepl("track", osm_df$fclass)]     <- "track"
  
  ### D. Remove certain classes
  osm_df <- osm_df[!(osm_df$fclass %in% c("steps",
                                          "unknown",
                                          "bridleway",
                                          "cycleway")),]
  
  ### E. Project
  osm_df <- osm_df %>% spTransform(CRS(cc_epsg))
  
  ### F. To SF
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
  dplyr::select(uid, country_code, year, urban_rural, latitude, longitude) %>%
  dplyr::filter(!is.na(latitude)) %>%
  dplyr::mutate(uid = uid %>% as.character)

country_codes_all <- survey_df$country_code %>% unique()

# 2. Load country_code to OSM dir data -----------------------------------------
# Make dataset that has [country_code] and [osm_root_name] (root name of OSM dir)

## Survey Details
survey_details_df <- read_xlsx(file.path(data_dir, SURVEY_NAME, "Survey Details", "survey_details.xlsx"))
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
buffer_m = 5000
country_code <- "TZ"

for(country_code in country_codes_all){
  
  OUT_PATH <- file.path(data_dir, SURVEY_NAME, "FinalData", 
                        "Individual Datasets", "osm", "roads_density",
                        paste0("osm_roaddensity_", buffer_m, "m_", country_code, ".Rds"))
  print(OUT_PATH)
  
  if(RE_EXTRACT_IF_EXISTS | !file.exists(OUT_PATH)){
    
    #### Grab projection for country i
    epsg_num <- survey_proj_df$epsg_projection[survey_proj_df$country_code %in% country_code]
    cc_epsg <- paste0("+init=epsg:",epsg_num)
    
    #### Load OSM
    osm_sf <- load_prep_osm_roads(country_code, osm_dir_df, cc_epsg)
    
    #### Prep survey data
    # Subset and buffer
    survey_df <- survey_df[survey_df$country_code %in% country_code,]
    
    survey_df <- survey_df %>%
      dplyr::select(uid, latitude, longitude)
    
    coordinates(survey_df) <- ~longitude+latitude
    crs(survey_df) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 "
    
    survey_buff_df <- geo.buffer_chunks(survey_df, r = buffer_m, chunk_size = 100)
    survey_buff_df <- survey_buff_df %>% spTransform(CRS(cc_epsg))
    
    survey_buff_sf <- survey_buff_df %>% st_as_sf()
    survey_buff_sf_agg <- survey_buff_sf %>% st_union()
    
    #### Extract density
    osm_density_country_i <- lapply(unique(osm_sf$fclass),
                                    extract_density_fclass_i,
                                    osm_sf, 
                                    survey_buff_sf, 
                                    survey_buff_sf_agg) %>%
      reduce(left_join, by = "uid")
    
    saveRDS(osm_density_country_i, OUT_PATH)
  }
}



