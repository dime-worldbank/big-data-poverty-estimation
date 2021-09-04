# Extract OSM Roads to BISP Households

buffer_size_osm_m <- c(5000)
SURVEY_NAME <- "DHS"

RE_EXTRACT_IF_EXISTS <- F

# Load OSM ---------------------------------------------------------------------
country_code = "IA"
buffer_size_m_i <- 5000
for(country_code in rev(c("BD", paste0("IA", 1:10), "KH", "KY", "MM", "NP", "PH", "PK", "TJ"))){
  for(buffer_size_m_i in buffer_size_osm_m){
    
    if(country_code %>% str_detect("IA")){
      subset_i <- country_code %>% str_replace_all("IA", "")
      
      OUT_PATH <- file.path(data_dir, SURVEY_NAME, 
                            "FinalData", "Individual Datasets", "osm", 
                            paste0("osm_",country_code,"_road_",buffer_size_m_i,"m_buff_SUBSET_",subset_i,".Rds"))
    } else{
      
      OUT_PATH <- file.path(data_dir, SURVEY_NAME, 
                            "FinalData", "Individual Datasets", "osm", 
                            paste0("osm_",country_code,"_road_",buffer_size_m_i,"m_buff.Rds"))
    }
    
    print(OUT_PATH)
    
    if(!file.exists(OUT_PATH) | RE_EXTRACT_IF_EXISTS){
      
      # Load Survey Data -------------------------------------------------------
      survey_df <- readRDS(file.path(data_dir, SURVEY_NAME, "FinalData", 
                                     "Individual Datasets", "survey_socioeconomic.Rds"))
      
      survey_df <- survey_df %>%
        dplyr::select(uid, country_code, year, urban_rural, latitude, longitude) %>%
        dplyr::filter(!is.na(latitude))
      
      # Set OSM Parameters -----------------------------------------------------
      if(country_code %in% "BD"){
        osm_country_dir <- "bangladesh-210101-free"
        UTM_PROJ <- BD_UTM_PROJ 
      } 
      if(country_code %>% str_detect("IA")){
        osm_country_dir <- "india-210101-free"
        UTM_PROJ <- IA_UTM_PROJ 
      } 
      if(country_code %in% "KH"){
        osm_country_dir <- "cambodia-210101-free"
        UTM_PROJ <- KH_UTM_PROJ 
      } 
      if(country_code %in% "KY"){
        osm_country_dir <- "kyrgyzstan-210101-free"
        UTM_PROJ <- KY_UTM_PROJ 
      } 
      if(country_code %in% "MM"){
        osm_country_dir <- "myanmar-210101-free"
        UTM_PROJ <- MM_UTM_PROJ 
      }
      if(country_code %in% "NP"){
        osm_country_dir <- "nepal-210101-free"
        UTM_PROJ <- NP_UTM_PROJ 
      }
      if(country_code %in% "PH"){
        osm_country_dir <- "philippines-210101-free"
        UTM_PROJ <- PH_UTM_PROJ 
      }
      if(country_code %in% "PK"){
        osm_country_dir <- "pakistan-210101-free"
        UTM_PROJ <- PK_UTM_PROJ 
      }
      if(country_code %in% "TJ"){
        osm_country_dir <- "tajikistan-210101-free"
        UTM_PROJ <- TJ_UTM_PROJ 
      }
      
      if(country_code %>% str_detect("IA")){
        
        osm_df <- readRDS(file.path(osm_dir, "FinalData", 
                                    osm_country_dir, 
                                    paste0("gis_osm_roads_free_1_SUBSET_",subset_i,".Rds")))
        
      } else{
        osm_df <- readRDS(file.path(osm_dir, "FinalData", 
                                    osm_country_dir, "gis_osm_roads_free_1.Rds"))
      }
      
      # Prep Data --------------------------------------------------------------
      #### Survey
      country_code_clean <- country_code %>% substring(1,2)
      survey_df <- survey_df[survey_df$country_code %in% country_code_clean,]
      coordinates(survey_df) <- ~longitude+latitude
      crs(survey_df) <- "+init=epsg:4326"
      
      #survey_sf <- survey_df %>% st_as_sf()
      survey_df <- survey_df %>% spTransform(UTM_PROJ)
      
      #### OSM
      ## Rename
      osm_df$fclass <- osm_df$fclass %>% as.character()
      osm_df$fclass[grepl("trunk", osm_df$fclass)]     <- "trunk"
      osm_df$fclass[grepl("motorway", osm_df$fclass)]  <- "motorway"
      osm_df$fclass[grepl("primary", osm_df$fclass)]   <- "primary"
      osm_df$fclass[grepl("secondary", osm_df$fclass)] <- "secondary"
      osm_df$fclass[grepl("tertiary", osm_df$fclass)]  <- "tertiary"
      osm_df$fclass[grepl("track", osm_df$fclass)]     <- "track"
      
      ## Remove
      osm_df <- osm_df[!(osm_df$fclass %in% c("steps",
                                              "unknown",
                                              "bridleway",
                                              "cycleway")),]
      
      ## Add compass bearing
      # osm_df$bearing <- lapply(1:nrow(osm_df), function(i){
      #   if((i %% 1000) == 0) print(i)
      #   
      #   osm_coords_df_i <- osm_df[i,] %>%
      #     coordinates() %>%
      #     map_df(as.data.frame)
      #   coordinates(osm_coords_df_i) <- ~X1 + X2
      #   crs(osm_coords_df_i) <- CRS("+init=epsg:4326")
      #   
      #   bearing_i <- bearing(osm_coords_df_i) 
      #   dist_i <- distGeo(osm_coords_df_i) 
      #   
      #   out <- weighted.mean(bearing_i, dist_i, na.rm=T)
      #   
      #   return(out)
      # }) %>% 
      #   unlist() %>%
      #   abs()
      
      ## Project
      osm_df <- osm_df %>% spTransform(UTM_PROJ)
      osm_sf <- osm_df %>% st_as_sf()
      
      # Length of Roads of Different Types -------------------------------------------
      extract_road_length <- function(road_type, osm_roads_sf, survey_sf, buffer_sf){
        
        print(paste(road_type," ---------------------------------------------------"))
        
        ## Subset roads to road type and cut up so only in buffered portions
        buffer_sf_agg <- buffer_sf %>% st_union()
        if(road_type != "all"){
          osm_roads_sf_typei <- osm_roads_sf[osm_roads_sf$fclass %in% road_type,]
        }
        osm_roads_sf_typei_subset <- st_intersection_chunks(osm_roads_sf_typei, buffer_sf_agg, 2000)
        
        ## Loop through observations and extract road length and distance
        road_length_dist_df <- map_df(1:nrow(buffer_sf), function(i){
          print(paste0(i, " - Extracting: ", road_type))
          
          buffer_sf_i <- buffer_sf[i,]
          survey_sf_i <- survey_sf[i,]
          
          #### Length
          intersects_tf <- st_intersects(buffer_sf_i, osm_roads_sf_typei_subset, sparse = F) %>% as.vector()
          road_in_buffer <- st_intersection(osm_roads_sf_typei_subset[intersects_tf,], buffer_sf_i)
          
          if(nrow(road_in_buffer) > 0){
            length_out <- road_in_buffer %>% st_length() %>% sum() %>% as.numeric() 
            
            #bearing_sd_out <- weighted.sd(road_in_buffer$bearing,length_out) # standard deviation, weighted by length
            
            N_segments_out <- road_in_buffer %>% nrow()
          } else{
            length_out <- 0
            #bearing_sd_out <- 0
            N_segments_out <- 0
          }
          
          #### Distance
          dist_out <- st_distance(survey_sf_i, osm_roads_sf_typei) %>% min()
          
          return(data.frame(length = length_out,
                            #bearing_sd = bearing_sd_out,
                            N_segments = N_segments_out,
                            dist = dist_out))
        }) 
        
        ## Cleanup output
        buffer_sf$geometry <- NULL
        buffer_sf <- buffer_sf %>%
          dplyr::select(uid)
        
        buffer_sf[[paste0(road_type, "_length")]]   <- road_length_dist_df$length
        buffer_sf[[paste0(road_type, "_distance")]] <- road_length_dist_df$dist
        buffer_sf[[paste0(road_type, "_N_segments")]] <- road_length_dist_df$N_segments
        #buffer_sf[[paste0(road_type, "_bearing_sd")]] <- road_length_dist_df$bearing_sd
        
        return(buffer_sf)
      }
      
      # Implement and Export ---------------------------------------------------------
      
      print(paste(buffer_size_m_i, "---------------------------------------------"))
      
      ## Survey into chunks
      survey_df$chunk <- rep(x = 1:999999, # make large number
                             each = 1000, # chunk size
                             length.out = nrow(survey_df))
      
      ## Buffer survey
      survey_df_buff  <- gBuffer(survey_df, width = buffer_size_m_i, byid = T) %>% st_as_sf()
      survey_sf <- survey_df %>% st_as_sf()
      
      ## Loop over chunks and append
      rd_length_m_df <- map_df(unique(survey_df_buff$chunk), function(chunk_i){
        
        ## Loop over road types and merge
        rd_length_m_df_i <- lapply(unique(osm_sf$fclass),
                                   extract_road_length,
                                   osm_sf,
                                   survey_sf[survey_sf$chunk %in% chunk_i,],
                                   survey_df_buff[survey_df_buff$chunk %in% chunk_i,]) %>%
          reduce(merge, by = "uid") %>%
          dplyr::rename_at(vars(-uid), ~ paste0("osm_", ., "_",buffer_size_m_i,"m_buff"))
        
        return(rd_length_m_df_i)
      })
      
      ## Save Data
      saveRDS(rd_length_m_df, OUT_PATH)
    }
  }
}

