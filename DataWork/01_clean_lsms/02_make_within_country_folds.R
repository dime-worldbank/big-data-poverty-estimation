# Clean DHS Data
# Variable Construction

set.seed(42)

over_nearest <- function(sp_i, gadm_i){
  # For each sp_i, takes the nearest polygon from gadm_i. Returns data frame
  # of sp_i with gadm data merged in
  
  # Add uids in same order as rows
  gadm_i$gadm_uid <- 1:nrow(gadm_i)
  
  # gadm uid, where intersect
  sp_i$gadm_uid <- over(sp_i, gadm_i)$gadm_uid
  
  # determine gadm uid of nearest (if doesn't intersect)
  sp_nonintersect_i <- sp_i[is.na(sp_i$gadm_uid),]
  
  if(nrow(sp_nonintersect_i) > 0){
    gadm_uid_nonintersect <- lapply(1:nrow(sp_nonintersect_i), function(i){
      print(paste(i, "/", nrow(sp_nonintersect_i)))
      gDistance(sp_nonintersect_i[i,],
                gadm_i,
                byid = T) %>% which.min()
    }) %>% unlist()
    
    sp_i$gadm_uid[is.na(sp_i$gadm_uid)] <- gadm_uid_nonintersect
  }
  
  # Add GADM data
  df_i <- as.data.frame(sp_i)
  df_i <- merge(df_i, gadm_i@data, by = "gadm_uid")
  
  return(df_i)
}

# Load Data --------------------------------------------------------------------
dhs_all_df_coll <- readRDS(file.path(lsms_dir, "FinalData", "Individual Datasets", 
                                     "survey_socioeconomic_varconstructed_tmp.Rds"))

c_dhs <- unique(dhs_all_df_coll$country_code) %>% sort()

# Within Country Folds ---------------------------------------------------------
dhs_gadm_cw <- bind_rows(
  data.frame(code_dhs = "MW", code_gadm = "MWI"), #### Malawi
  data.frame(code_dhs = "BF", code_gadm = "BFA"),
  data.frame(code_dhs = "BJ", code_gadm = "BEN"),
  data.frame(code_dhs = "ET", code_gadm = "ETH"),
  data.frame(code_dhs = "TG", code_gadm = "TGO"),
  data.frame(code_dhs = "CI", code_gadm = "CIV")) %>%
  mutate(code_dhs = code_dhs %>% as.character,
         code_gadm = code_gadm %>% as.character)

dhs_all_df_coll_folds <- map_df(unique(dhs_all_df_coll$country_code), function(cc_dhs){
  print(cc_dhs)
  
  cc_gadm <- dhs_gadm_cw$code_gadm[dhs_gadm_cw$code_dhs %in% cc_dhs]
  
  df_i <- dhs_all_df_coll[dhs_all_df_coll$country_code %in% cc_dhs,]
  gadm_i <- readRDS(file.path(data_dir, "GADM", "FinalData", "adm2", paste0("gadm36_",cc_gadm,"_2_sp.rds")))
  
  gadm_i[,c("GID_0", "GID_1", "GID_2",
            "NAME_0", "NAME_1", "NAME_2")]
  
  # Random fold
  within_country_fold <- rep_len(1:5, length.out = nrow(gadm_i)) %>% sample()
  gadm_i$within_country_fold <- paste0(cc_dhs, "_", within_country_fold)
  
  sp_i <- df_i
  coordinates(sp_i) <- ~longitude+latitude
  crs(sp_i) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
  
  df_out <- over_nearest(sp_i, gadm_i)
  
  return(df_out)
}) 

# Select variables -------------------------------------------------------------
dhs_all_df_coll_folds <- dhs_all_df_coll_folds %>%
  dplyr::select(uid, 
                GID_0,
                GID_1,
                GID_2,
                NAME_0,
                NAME_1,
                NAME_2,
                within_country_fold,
                gadm_uid)

# Export -----------------------------------------------------------------------
saveRDS(dhs_all_df_coll_folds, file.path(lsms_dir, "FinalData", "Individual Datasets", 
                                         "survey_fold.Rds"))



