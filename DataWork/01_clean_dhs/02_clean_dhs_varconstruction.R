# Clean DHS Data
# Variable Construction

set.seed(42)

over_nearest <- function(sp_i, gadm_i){
  # For each sp_i, takes the nearest polygon from gadm_i. Returns data frame
  # ob sp_i with gadm data merged in
  
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
dhs_all_df <- readRDS(file.path(dhs_dir, "FinalData", "Individual Datasets", "survey_socioeconomic_hhlevel.Rds"))

## Subset - needs coordinates
dhs_all_df <- dhs_all_df %>%
  dplyr::filter(!is.na(latitude))

# Add variable most recent and subset to most recent ---------------------------
dhs_all_df <- dhs_all_df %>%
  dplyr::group_by(country_code) %>%
  dplyr::mutate(latest_survey_country = max(year)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(most_recent_survey = latest_survey_country == year) %>%
  dplyr::select(-latest_survey_country)

dhs_all_df <- dhs_all_df %>%
  dplyr::filter(most_recent_survey %in% T)

# Deal with missing/don't know codes -------------------------------------------
# e.g., value of 99 that means "don't know" -- these should be NA

#### Time to get water
# 996; at source --> 0
# 998/999; don't know -->NA
dhs_all_df$water_time_to_get[dhs_all_df$water_time_to_get %in% 996] <- 0
dhs_all_df$water_time_to_get[dhs_all_df$water_time_to_get %in% c(998,999)] <- NA

#### Assets
dhs_all_df$has_bank_account[dhs_all_df$has_bank_account %in% 8:9] <- NA
dhs_all_df$has_car[dhs_all_df$has_car %in% 9] <- NA
dhs_all_df$has_electricity[dhs_all_df$has_electricity %in% 9] <- NA
dhs_all_df$has_fridge[dhs_all_df$has_fridge %in% 9] <- NA
dhs_all_df$has_motorbike[dhs_all_df$has_motorbike %in% 9] <- NA
dhs_all_df$has_radio[dhs_all_df$has_radio %in% 9] <- NA
dhs_all_df$has_tv[dhs_all_df$has_tv %in% 9] <- NA

# Asset Index ------------------------------------------------------------------
# (1) High values = wealthier, lower values = poorer
# (2) Remove NAs

##### ** Prep Variables #####

## Number of sleeping rooms; if 0, say 1
dhs_all_df <- dhs_all_df %>%
  dplyr::mutate(n_rooms_sleeping = n_rooms_sleeping %>% as.numeric(),
                
                n_rooms_sleeping = case_when(
                  n_rooms_sleeping %in% 0 ~ 1,
                  n_rooms_sleeping %in% 99 ~ NA_real_,
                  TRUE ~ n_rooms_sleeping
                ),
                
                n_sleeping_rooms_pp = n_hh_members / n_rooms_sleeping,
                
                n_sleeping_rooms_pp_cat = case_when(
                  (n_sleeping_rooms_pp >= 0 & n_sleeping_rooms_pp < 1) ~ 3,
                  (n_sleeping_rooms_pp >= 1 & n_sleeping_rooms_pp < 2) ~ 2,
                  (n_sleeping_rooms_pp >= 2) ~ 1
                ))

## Floor material
dhs_all_df$floor_material_cat <- floor(as.numeric(dhs_all_df$floor_material)/10)
dhs_all_df$floor_material_cat[dhs_all_df$floor_material_cat >= 4] <- NA

## Wall material
dhs_all_df$wall_material_cat <- floor(as.numeric(dhs_all_df$wall_material)/10)
dhs_all_df$wall_material_cat[dhs_all_df$wall_material_cat >= 4] <- NA

## Roof material
dhs_all_df$roof_material_cat <- floor(as.numeric(dhs_all_df$roof_material)/10)
dhs_all_df$roof_material_cat[dhs_all_df$roof_material_cat >= 4] <- NA

## Water source
# 11 = piped into dwelling
dhs_all_df$water_source_piped_dwelling <- as.numeric(dhs_all_df$water_source == 11)

## Toilet Type
dhs_all_df$flush_toilet_sewer <- as.numeric(dhs_all_df$toilet_type == 11)

## Time to get drinking water
dhs_all_df <- dhs_all_df %>%
  dplyr::mutate(water_time_to_get_cat = case_when(
    water_time_to_get == 0 ~ 3,
    water_time_to_get > 0 & water_time_to_get <= 30 ~ 2,
    water_time_to_get > 30 ~ 1
  ))

## Years of Education
dhs_all_df$educ_years_hh_max_scale <- scale(dhs_all_df$educ_years_hh_max) %>%
  as.numeric()

## NAs
if(F){
  na_df <- dhs_all_df %>%
    dplyr::select(n_sleeping_rooms_pp_cat, 
                  floor_material_cat,
                  wall_material_cat, 
                  roof_material_cat, 
                  water_source_piped_dwelling, 
                  flush_toilet_sewer,
                  has_electricity,
                  has_radio,
                  has_tv,
                  has_fridge,
                  has_motorbike,
                  has_car) %>%
    dplyr::mutate_all(is.na) %>%
    dplyr::summarise_all(sum) %>%
    t %>%
    as.data.frame()
  
  na_df$prop <- na_df$V1 / nrow(dhs_all_df) 
}

##### ** Variable sets for PCA #####

# Version without roof_material due to high number of missing values
pca_allvars <- c("has_electricity",
                 "has_tv",
                 "has_fridge",
                 "has_motorbike",
                 "has_car",
                 "floor_material_cat",
                 "wall_material_cat",
                 "roof_material_cat",
                 "water_time_to_get_cat",
                 "water_source_piped_dwelling",
                 "flush_toilet_sewer",
                 "n_sleeping_rooms_pp_cat",
                 "educ_years_hh_max_scale")

pca_allvars_noroof <- c("has_electricity",
                        "has_tv",
                        "has_fridge",
                        "has_motorbike",
                        "has_car",
                        "floor_material_cat",
                        "wall_material_cat",
                        "water_time_to_get_cat",
                        "water_source_piped_dwelling",
                        "flush_toilet_sewer",
                        "n_sleeping_rooms_pp_cat",
                        "educ_years_hh_max_scale")

pca_physicalvars <- c("has_electricity",
                      "floor_material_cat",
                      "wall_material_cat",
                      "roof_material_cat")

pca_physicalvars_noroof <- c("has_electricity",
                             "floor_material_cat",
                             "wall_material_cat")

pca_nonphysicalvars <- c("has_tv",
                         "has_fridge",
                         "has_motorbike",
                         "has_car",
                         "water_time_to_get_cat",
                         "water_source_piped_dwelling",
                         "flush_toilet_sewer",
                         "n_sleeping_rooms_pp_cat",
                         "educ_years_hh_max_scale")

##### ** Compute PCA #####
dhs_all_df$ind_id <- 1:nrow(dhs_all_df)

compute_pca_impute_missing <- function(pca_vars, dhs_all_df){
  
  pca_df <- dhs_all_df %>%
    dplyr::select(all_of(pca_vars)) %>%
    mutate_all(as.numeric) 
  
  # file:///Users/robmarty/Downloads/v70i01.pdf
  # Estimate number of dimensions; takes a while, so take a random sample
  N_use <- ceiling(nrow(pca_df)*0.1)
  
  ncomp <- pca_df %>%
    arrange(runif(n())) %>%
    head(N_use) %>%
    estim_ncpPCA()
  
  #ncomp <- estim_ncpPCA(pca_df)
  res.imp <- imputePCA(pca_df, ncp = ncomp$ncp)
  res.pca <- prcomp(res.imp$completeObs, scale = T)
  out <- res.pca$x[,1]*(-1)
  
  return(out)
}

compute_pca_rm_na <- function(pca_vars, var_name, dhs_all_df){
  # Compute PCA where removing missing values
  
  pca_df <- dhs_all_df %>% 
    dplyr::select(all_of(c("ind_id", pca_vars))) %>%
    drop_na()
  
  pca_obj <- pca_df %>%
    dplyr::select(-ind_id) %>%
    prcomp(scale = T)
  
  out_df <- data.frame(pca = pca_obj$x[,1]*(-1),
                       ind_id = pca_df$ind_id)
  
  names(out_df)[1] <- var_name
  
  return(out_df)
}

## PCA - Removing NAs
dhs_all_df <- dhs_all_df %>%
  left_join(compute_pca_rm_na(pca_allvars, "pca_allvars_rmna", dhs_all_df), 
            by = "ind_id") %>%
  left_join(compute_pca_rm_na(pca_allvars_noroof, "pca_allvars_noroof_rmna", dhs_all_df), 
            by = "ind_id") %>%
  left_join(compute_pca_rm_na(pca_physicalvars, "pca_physicalvars_rmna", dhs_all_df), 
            by = "ind_id") %>%
  left_join(compute_pca_rm_na(pca_physicalvars_noroof, "pca_physicalvars_noroof_rmna", dhs_all_df), 
            by = "ind_id") %>%
  left_join(compute_pca_rm_na(pca_nonphysicalvars, "pca_nonphysicalvars_rmna", dhs_all_df), 
            by = "ind_id")

## PCA - Imputing Missing NAs
dhs_all_df$pca_allvars             <- compute_pca_impute_missing(pca_allvars, dhs_all_df)
dhs_all_df$pca_allvars_noroof      <- compute_pca_impute_missing(pca_allvars_noroof, dhs_all_df)
dhs_all_df$pca_physicalvars        <- compute_pca_impute_missing(pca_physicalvars, dhs_all_df)
dhs_all_df$pca_physicalvars_noroof <- compute_pca_impute_missing(pca_physicalvars_noroof, dhs_all_df)
dhs_all_df$pca_nonphysicalvars     <- compute_pca_impute_missing(pca_nonphysicalvars, dhs_all_df)

a <- dhs_all_df[dhs_all_df$country_code %in% "IA",]
for(var in c("pca_allvars_rmna",
             "pca_allvars_noroof_rmna",
             "pca_physicalvars_rmna",
             "pca_physicalvars_noroof_rmna",
             "pca_nonphysicalvars_rmna",

             "pca_allvars",
             "pca_allvars_noroof",
             "pca_physicalvars",
             "pca_physicalvars_noroof",
             "pca_nonphysicalvars")){
  print(var)
  cor.test(dhs_all_df$wealth_index_score, dhs_all_df[[var]]) %>% print()
  cor.test(a$wealth_index_score, a[[var]]) %>% print()
  Sys.sleep(2)
}

# Aggregate --------------------------------------------------------------------
dhs_all_df_coll <- dhs_all_df %>%
  group_by(uid, country_code, country_year, urban_rural, year, most_recent_survey,
           latitude, longitude) %>%
  summarise_if(is.numeric, mean, na.rm=T) %>%
  ungroup()

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
  data.frame(code_dhs = "LB", code_gadm = "LBN"),
  data.frame(code_dhs = "LS", code_gadm = "LSO"),
  data.frame(code_dhs = "MA", code_gadm = "MAR"),
  data.frame(code_dhs = "MB", code_gadm = "MDA"), # Moldova
  data.frame(code_dhs = "MD", code_gadm = "MDG"), ### Madagascar?
  data.frame(code_dhs = "ML", code_gadm = "MLI"), ### Mali
  data.frame(code_dhs = "MM", code_gadm = "MMR"),
  data.frame(code_dhs = "MW", code_gadm = "MWI"), #### Malawi
  data.frame(code_dhs = "MZ", code_gadm = "MOZ"),
  data.frame(code_dhs = "NG", code_gadm = "NGA"),
  data.frame(code_dhs = "NM", code_gadm = "NAM"), # Namibia
  data.frame(code_dhs = "NP", code_gadm = "NPL"),
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

dhs_all_df_coll <- map_df(unique(dhs_all_df$country_code), function(cc_dhs){
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

# Country Name -----------------------------------------------------------------
dhs_all_df_coll$country_name <- countrycode(dhs_all_df_coll$country_code, origin = "iso2c", destination = "country.name")

dhs_all_df_coll <- dhs_all_df_coll %>%
  mutate(country_name = case_when(
    country_code == "BU" ~ "Burundi",
    country_code == "DR" ~ "Dominican Republic",
    country_code == "IA" ~ "India",
    country_code == "NM" ~ "Namibia",
    TRUE ~ country_name))

# Export -----------------------------------------------------------------------
## All
saveRDS(dhs_all_df_coll, file.path(dhs_dir, "FinalData", "Individual Datasets", "survey_socioeconomic.Rds"))
write.csv(dhs_all_df_coll, file.path(dhs_dir, "FinalData", "Individual Datasets", "survey_socioeconomic.csv"), row.names = F)

saveRDS(dhs_all_df_coll, file.path(gdrive_dir, "Data", "DHS", "FinalData", "Individual Datasets", "survey_socioeconomic.Rds"))
write.csv(dhs_all_df_coll, file.path(gdrive_dir, "Data", "DHS", "FinalData", "Individual Datasets", "survey_socioeconomic.csv"), row.names = F)

saveRDS(dhs_all_df_coll, file.path(secure_dir, "Data", "DHS",  "FinalData - PII", "survey_socioeconomic_geo.Rds"))
write.csv(dhs_all_df_coll, file.path(secure_dir, "Data", "DHS",  "FinalData - PII", "survey_socioeconomic_geo.csv"), row.names = F)

## Geo Only
df_geoonly <- dhs_all_df_coll %>%
  dplyr::select(uid, latitude, longitude, urban_rural, most_recent_survey, country_code, year)

saveRDS(df_geoonly, file.path(secure_dir, "Data", "DHS",  "FinalData - PII", "GPS_uid_crosswalk.Rds"))
write.csv(df_geoonly, file.path(secure_dir, "Data", "DHS",  "FinalData - PII", "GPS_uid_crosswalk.csv"), row.names = F)

