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
dhs_all_df_coll <- readRDS(file.path(dhs_nga_exp_dir, "FinalData", "Individual Datasets", 
                                     "survey_socioeconomic_varconstructed_tmp.Rds"))

c_dhs <- unique(dhs_all_df_coll$country_code) %>% sort()

# Within Country Folds ---------------------------------------------------------
dhs_gadm_cw <- bind_rows(
  data.frame(code_dhs = "AL", code_gadm = "ALB"),
  data.frame(code_dhs = "AM", code_gadm = "ARM"),
  data.frame(code_dhs = "AO", code_gadm = "AGO"),
  data.frame(code_dhs = "BD", code_gadm = "BGD"),
  data.frame(code_dhs = "BF", code_gadm = "BFA"),
  data.frame(code_dhs = "BJ", code_gadm = "BEN"),
  data.frame(code_dhs = "BO", code_gadm = "BOL"),
  data.frame(code_dhs = "BU", code_gadm = "BDI"), # Burundi
  data.frame(code_dhs = "CD", code_gadm = "COD"),
  data.frame(code_dhs = "CI", code_gadm = "CIV"),
  data.frame(code_dhs = "CM", code_gadm = "CMR"),
  data.frame(code_dhs = "CO", code_gadm = "COL"),
  data.frame(code_dhs = "DR", code_gadm = "DOM"), # Dominican Republic
  data.frame(code_dhs = "EG", code_gadm = "EGY"),
  data.frame(code_dhs = "ET", code_gadm = "ETH"),
  data.frame(code_dhs = "GA", code_gadm = "GAB"),
  data.frame(code_dhs = "GH", code_gadm = "GHA"),
  data.frame(code_dhs = "GM", code_gadm = "GMB"),
  data.frame(code_dhs = "GN", code_gadm = "GIN"),
  data.frame(code_dhs = "GU", code_gadm = "GTM"), # Guatemala
  data.frame(code_dhs = "GY", code_gadm = "GUY"),
  data.frame(code_dhs = "HN", code_gadm = "HND"),
  data.frame(code_dhs = "HT", code_gadm = "HTI"),
  data.frame(code_dhs = "IA", code_gadm = "IND"),
  data.frame(code_dhs = "ID", code_gadm = "IDN"),
  data.frame(code_dhs = "JO", code_gadm = "JOR"),
  data.frame(code_dhs = "KE", code_gadm = "KEN"),
  data.frame(code_dhs = "KH", code_gadm = "KHM"),
  data.frame(code_dhs = "KM", code_gadm = "COM"),
  data.frame(code_dhs = "KY", code_gadm = "KGZ"),
  data.frame(code_dhs = "LB", code_gadm = "LBR"),
  data.frame(code_dhs = "LS", code_gadm = "LSO"),
  data.frame(code_dhs = "MA", code_gadm = "MAR"),
  data.frame(code_dhs = "MB", code_gadm = "MDA"), # Moldova
  data.frame(code_dhs = "MD", code_gadm = "MDG"), ### Madagascar?
  data.frame(code_dhs = "ML", code_gadm = "MLI"), ### Mali
  data.frame(code_dhs = "MM", code_gadm = "MMR"),
  data.frame(code_dhs = "MW", code_gadm = "MWI"), #### Malawi
  data.frame(code_dhs = "MZ", code_gadm = "MOZ"),
  data.frame(code_dhs = "NG", code_gadm = "NGA"),
  data.frame(code_dhs = "NI", code_gadm = "NER"), # Niger
  data.frame(code_dhs = "NM", code_gadm = "NAM"), # Namibia
  data.frame(code_dhs = "NP", code_gadm = "NPL"),
  data.frame(code_dhs = "PE", code_gadm = "PER"),
  data.frame(code_dhs = "PH", code_gadm = "PHL"),
  data.frame(code_dhs = "PK", code_gadm = "PAK"),
  data.frame(code_dhs = "RW", code_gadm = "RWA"),
  data.frame(code_dhs = "SL", code_gadm = "SLE"),
  data.frame(code_dhs = "SN", code_gadm = "SEN"),
  data.frame(code_dhs = "SZ", code_gadm = "SWZ"),
  data.frame(code_dhs = "TD", code_gadm = "TCD"),
  data.frame(code_dhs = "TG", code_gadm = "TGO"),
  data.frame(code_dhs = "TJ", code_gadm = "TJK"),
  data.frame(code_dhs = "TL", code_gadm = "TLS"),
  data.frame(code_dhs = "TZ", code_gadm = "TZA"),
  data.frame(code_dhs = "UG", code_gadm = "UGA"),
  data.frame(code_dhs = "ZA", code_gadm = "ZAF"),
  data.frame(code_dhs = "ZM", code_gadm = "ZMB"),
  data.frame(code_dhs = "ZW", code_gadm = "ZWE")
) %>%
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

# # Select variables -------------------------------------------------------------
# dhs_all_df_coll_folds <- dhs_all_df_coll_folds %>%
#   dplyr::select(uid, 
#                 GID_0,
#                 GID_1,
#                 GID_2,
#                 NAME_0,
#                 NAME_1,
#                 NAME_2,
#                 within_country_fold,
#                 gadm_uid)

# Export -----------------------------------------------------------------------
#### All Years
saveRDS(dhs_all_df_coll_folds, file.path(dhs_nga_exp_dir, "FinalData", "Individual Datasets", "survey_socioeconomic.Rds"))

write.csv(dhs_all_df_coll_folds,
          file.path(dhs_nga_exp_dir, "FinalData", "Individual Datasets", "survey_socioeconomic.csv"),
          row.names = F)

# #### Only for predictions
# saveRDS(dhs_all_df_coll_folds[dhs_all_df_coll_folds$year %in% 2013,], 
#         file.path(dhs_exp_dir, "FinalData", "Individual Datasets", "survey_socioeconomic.Rds"))
# 
# write.csv(dhs_all_df_coll_folds[dhs_all_df_coll_folds$year %in% 2013,],
#           file.path(dhs_exp_dir, "FinalData", "Individual Datasets", "survey_socioeconomic.csv"),
#           row.names = F)



#### CHECK PCA
dhs_df <- readRDS(file.path(dhs_dir, "FinalData", "Individual Datasets", "survey_socioeconomic.Rds"))
dhs_df <- dhs_df %>%
  dplyr::select(uid, pca_allvars)

dhs_df <- dhs_df %>%
  left_join(dhs_all_df_coll_folds %>% 
              dplyr::rename(pca_allvars_new = pca_allvars),
            by = "uid")

cor.test(dhs_df$pca_allvars, dhs_df$pca_allvars_new)
plot(dhs_df$pca_allvars, dhs_df$pca_allvars_new)





df <- dhs_all_df_coll_folds %>%
  group_by(NAME_1, year) %>%
  dplyr::summarise(pca_allvars = mean(pca_allvars)) %>%
  dplyr::mutate(year = paste0("yr", year)) %>%
  pivot_wider(id_cols = NAME_1, values_from = pca_allvars, names_from = year) %>%
  
  dplyr::mutate(yr2013_est = (yr2008 - yr2003) + yr2008)

cor.test(df$yr2013, df$yr2013_est)
df %>%
  ggplot() +
  geom_point(aes(x = yr2013,
                 y = yr2013_est))


