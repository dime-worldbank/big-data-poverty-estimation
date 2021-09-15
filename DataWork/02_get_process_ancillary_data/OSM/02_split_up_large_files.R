# Split up large OSM files to facilitate processing

# India OSM file is large, so split up to facilitate faster processing

# Load data --------------------------------------------------------------------
osm_country_dir <- "india-210101-free"
osm_df <- readRDS(file.path(osm_dir, "FinalData", 
                            osm_country_dir, "gis_osm_roads_free_1.Rds"))

N_SUBSETS <- 30

group_vec <- rep_len(x = 1:N_SUBSETS, length.out = nrow(osm_df))

for(i in 1:N_SUBSETS){
  print(i)
  
  osm_df_i <- osm_df[group_vec %in% i,]
  
  saveRDS(osm_df_i,
          file.path(osm_dir, "FinalData", 
                    osm_country_dir, paste0("gis_osm_roads_free_1_SUBSET_",i,".Rds")))
}



