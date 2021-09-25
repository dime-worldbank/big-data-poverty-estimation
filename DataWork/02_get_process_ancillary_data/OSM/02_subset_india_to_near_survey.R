# Subset India road network file to points near roads

re_extract_if_exists <- F

# Load / Prep Data -------------------------------------------------------------
#### Survey data
survey_df <- readRDS(file.path(data_dir, SURVEY_NAME,
                               "FinalData", "Individual Datasets", 
                               "survey_socioeconomic.Rds"))

survey_df <- survey_df[survey_df$country_code %in% "IA",]

#### GADM
gadm <- readRDS(file.path(gadm_dir, "FinalData", "adm2", "gadm36_iND_2_sp.Rds"))
gadm$id <- 1
gadm@data <- gadm@data %>% 
  dplyr::select(id, GID_2)

#### Roads
osm_df <- readRDS(file.path(osm_dir, "FinalData", "india-210101-free", "gis_osm_roads_free_1.Rds"))

osm_df <- osm_df[!(osm_df$fclass %in% c("steps",
                                        "unknown",
                                        "bridleway",
                                        "cycleway")),]

# Extract and save chunks ------------------------------------------------------
for(gid_i in sort(unique(gadm$GID_2))){
  print(gid_i)
  
  OUT_PATH <- file.path(osm_dir, "FinalData", "india-210101-free", 
                        paste0("gis_osm_roads_free_1_SUBSET_",gid_i,".Rds"))
  
  if(!file.exists(OUT_PATH) | re_extract_if_exists){
    
    ## Subset
    gadm_i <- gadm[gadm$GID_2 %in% gid_i,]
    gadm_i$GID_2 <- NULL
    
    ## Buffer
    # 161 km is roughly 100 miles
    gadm_buff_i <- gBuffer(gadm_i, width = 161/111.12, byid=T) 
    
    #gadm_buff_i_box <- rgeos::bbox2SP(bbox = gadm_buff_i %>% bbox(),
    #                                  proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
    #gadm_buff_i_box$id <- 1
    
    ## Restrict Roads
    osm_df_OVER_gadm <- over_chunks(osm_df, gadm_buff_i, fn_type = "none", chunk_size = 7000)
    osm_df_in_gadm <- osm_df[osm_df_OVER_gadm$id %in% 1,]
    
    ## Save
    saveRDS(osm_df_in_gadm, 
            file.path(osm_dir, "FinalData", "india-210101-free", 
                      paste0("gis_osm_roads_free_1_SUBSET_",gid_i,".Rds")))
  }
}



