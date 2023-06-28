# Extract NTL to GADM 1

# Load / prep ADM --------------------------------------------------------------
adm1_sp <- file.path(gadm_dir, "RawData") %>% 
  list.files(full.names = T,
             pattern = "*.rds") %>%
  str_subset("_0_") %>%
  lapply(readRDS) %>%
  do.call(what = "rbind")

adm1_sp@data <- adm1_sp@data %>%
  dplyr::select(GID_0)

# Extract data -----------------------------------------------------------------
ntl_all_df <- map_df(1992:2020, function(year_i){
  print(year_i)
  
  if(year_i <= 1992) year_i <- 1992
  if(year_i >= 2020) year_i <- 2020
  
  if(year_i <= 2013){
    ntl <- raster(file.path(ntl_harmon_dir, "RawData", paste0("Harmonized_DN_NTL_",year_i,"_calDMSP.tif")))
  } else{
    ntl <- raster(file.path(ntl_harmon_dir, "RawData", paste0("Harmonized_DN_NTL_",year_i,"_simVIIRS.tif")))
  }
  
  adm1_sp$ntl_harmon <- exact_extract(x = ntl,
                                      y = adm1_sp,
                                      fun = "mean")
  
  adm1_sp$year <- year_i
  
  adm1_df <- adm1_sp@data %>%
    dplyr::select(GID_0, year, ntl_harmon)
  
  return(adm1_df)
})

# Export data ------------------------------------------------------------------
saveRDS(ntl_all_df, file.path(ntl_harmon_dir, "FinalData", "ntl_harmon_gadm0.Rds"))



