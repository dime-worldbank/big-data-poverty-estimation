# Download GADM

# Some countries have no ADM2. In these cases, use ADM1, but create an ADM2 variable
# that just uses ADM1. 

only_adm1_avail <- c("ARM", "COM", "LSO", "MDA")

iso_codes <- list.files(file.path(data_dir, "GADM", "RawData"),
                        pattern = "*_sp.rds") %>%
  str_replace_all("gadm36_", "") %>%
  str_replace_all("_[:digit:]_sp.rds", "") %>% 
  unique()

for(iso_code_i in iso_codes){
  
  if(iso_code_i %in% only_adm1_avail){
    gadm_i <- readRDS(file.path(data_dir, "GADM", "RawData", paste0("gadm36_", iso_code_i, "_1_sp.Rds")))
    
    gadm_i$NAME_2    <- gadm_i$NAME_1
    gadm_i$GID_2     <- gadm_i$GID_1
    gadm_i$NL_NAME_2 <- gadm_i$NL_NAME_1

    saveRDS(gadm_i, file.path(data_dir, "GADM", "FinalData", "adm2", paste0("gadm36_", iso_code_i, "_2_sp.Rds")))
  } else{
    gadm_i <- readRDS(file.path(data_dir, "GADM", "RawData", paste0("gadm36_", iso_code_i, "_2_sp.Rds")))
    saveRDS(gadm_i, file.path(data_dir, "GADM", "FinalData", "adm2", paste0("gadm36_", iso_code_i, "_2_sp.Rds")))
  }
  

}

