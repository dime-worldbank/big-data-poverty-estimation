# Extract OSM Roads to BISP Households

buffer_size_osm_m <- c(5000)
SURVEY_NAME <- "DHS"

RE_EXTRACT_IF_EXISTS <- F

for(buffer_size_osm_m in c(5000)){
  for(country_code in c("BD", "IA", "KH", "KY", "MM", "NP", "PH", "PK", "TJ")){
    print(country_code)
    
    OUT_PATH <- file.path(data_dir, SURVEY_NAME, 
                          "FinalData", "Individual Datasets", "osm", 
                          paste0("osm_",country_code,"_poibuildings_",buffer_size_osm_m,"m_buff.Rds"))
    
    if(!file.exists(OUT_PATH) | RE_EXTRACT_IF_EXISTS){
      
      # Country Variables ------------------------------------------------------
      if(country_code %in% "BD"){
        osm_country_dir <- "bangladesh-210101-free"
        UTM_PROJ <- BD_UTM_PROJ 
      } 
      if(country_code %in% "IA"){
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
      
      # Load Data ----------------------------------------------------------------
      survey_df <- readRDS(file.path(dhs_dir, "FinalData", "Individual Datasets", "survey_socioeconomic.Rds"))
      survey_df <- survey_df %>%
        dplyr::select(uid, country_code, year, urban_rural, latitude, longitude) %>%
        dplyr::filter(!is.na(latitude))
      
      survey_df <- survey_df[survey_df$country_code %in% country_code,]
      
      survey_df <- survey_df %>%
        dplyr::select(uid, latitude, longitude)
      
      coordinates(survey_df) <- ~longitude+latitude
      crs(survey_df) <- "+init=epsg:4326"
      
      survey_df <- survey_df %>% spTransform(UTM_PROJ)
      
      ## Buffered Version
      survey_buff_df <- survey_df %>%
        gBuffer(width = buffer_size_osm_m, 
                byid=T)
      survey_buff_df$one <- 1
      
      # Load OSM -----------------------------------------------------------------
      osm1_df <- readRDS(file.path(osm_dir, "FinalData", osm_country_dir, "gis_osm_pois_free_1.Rds"))
      osm2_df <- readRDS(file.path(osm_dir, "FinalData", osm_country_dir, "gis_osm_pois_a_free_1.Rds"))
      #osm_buildings_df <- readRDS(file.path(osm_dir, "FinalData", osm_country_dir, "gis_osm_buildings_a_free_1.Rds"))
      
      ## POI
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
      
      osm_df <- osm_df %>% spTransform(UTM_PROJ)
      osm_df$one <- 1
      
      ## Buildings
      if(F){
        osm_buildings_sp <- osm_buildings_df %>% 
          coordinates() %>% 
          as.data.frame() %>%
          dplyr::rename(longitude = 1,
                        latitude = 2) %>%
          bind_cols(osm_buildings_df@data)
        
        coordinates(osm_buildings_sp) <- ~longitude+latitude
        crs(osm_buildings_sp) <- CRS("+init=epsg:4326")
        
        osm_buildings_sp <- osm_buildings_sp %>% spTransform(UTM_PROJ)
        osm_buildings_sp$one <- 1
      }
      
      # Distance to Class --------------------------------------------------------
      for(class_i in unique(osm_df$fclass[!is.na(osm_df$fclass)])){
        print(class_i)
        
        osm_df_classi <- osm_df[osm_df$fclass %in% class_i,]
        
        osm_df_classi_agg <- osm_df_classi %>%
          gBuffer(width = 0.00000001, byid=T) %>%
          st_as_sf() %>%
          st_union() %>%
          as("Spatial")
        
        survey_df[[paste0("osm_dist_poi_", class_i)]] <- gDistance_chunks(survey_df, osm_df_classi_agg, 100)
      } 
      
      # N Poi Nearby -------------------------------------------------------------
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
      
      # N Building Nearby --------------------------------------------------------
      if(F){
        buildings_df <- over(osm_buildings_sp, survey_buff_df)$uid %>%
          as.character() %>%
          as.data.frame() %>%
          dplyr::rename(uid = ".") %>%
          dplyr::filter(!is.na(uid)) %>%
          group_by(uid) %>%
          dplyr::summarise(N = n()) %>%
          ungroup() %>%
          dplyr::rename(osm_n_buildings = N)
        
        survey_df <- merge(survey_df, buildings_df, by = "uid", all.x=T)
        survey_df$osm_n_buildings[is.na(survey_df$osm_n_buildings)] <- 0
      }
      
      # Export Data --------------------------------------------------------------
      survey_df <- survey_df@data
      
      survey_df <- survey_df %>%
        dplyr::rename_at(vars(-uid), ~ paste0(., "_",buffer_size_osm_m,"m_buff"))
      
      ## Save Data
      saveRDS(survey_df, OUT_PATH)
    }
  }
}

