# Split up large OSM files to facilitate processing

# India OSM file is large, so split up to facilitate faster processing

# Load data --------------------------------------------------------------------
for(osm_country_dir in c("india-210101-free")){
  print(osm_country_dir)
  
  if(osm_country_dir %in% "india-210101-free") N_SUBSETS <- 250
  
  ## Delete existing subsets
  files_to_rm <- file.path(osm_dir, "FinalData", osm_country_dir) %>%
    list.files() %>%
    str_subset("_SUBSET_")
  
  for(file_to_rm_i in files_to_rm){
    file.path(osm_dir, "FinalData", osm_country_dir, file_to_rm_i) %>%
      file.remove()
  }
  
  ## Load data
  osm_df <- readRDS(file.path(osm_dir, "FinalData", 
                              osm_country_dir, "gis_osm_roads_free_1.Rds"))
  
  ## Determine subsets
  group_vec <- rep_len(x = 1:N_SUBSETS, length.out = nrow(osm_df))
  
  ## Save into different subsets
  for(i in 1:N_SUBSETS){
    print(i)
    
    osm_df_i <- osm_df[group_vec %in% i,]
    
    saveRDS(osm_df_i,
            file.path(osm_dir, "FinalData", 
                      osm_country_dir, paste0("gis_osm_roads_free_1_SUBSET_",i,".Rds")))
  }
  
}






