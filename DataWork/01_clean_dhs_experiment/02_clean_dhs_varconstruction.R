# Clean DHS Data
# Variable Construction

set.seed(42)

# Load data --------------------------------------------------------------------
dhs_all_df <- readRDS(file.path(dhs_exp_dir, "FinalData", "Individual Datasets", 
                                "survey_socioeconomic_hhlevel.Rds"))

# Subset data ------------------------------------------------------------------
## Subset - needs coordinates
dhs_all_df <- dhs_all_df %>%
  dplyr::filter(!is.na(latitude),
                year >= 1998)

# Add (a) most recent and (b) oldest survey variables --------------------------
dhs_all_df <- dhs_all_df %>%
  dplyr::group_by(country_code) %>%
  dplyr::mutate(survey_year_max = max(year),
                survey_year_min = min(year)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(most_recent_survey = survey_year_max == year,
                oldest_survey = survey_year_min == year) %>%
  dplyr::select(-c(survey_year_min, survey_year_max))
# 
# ## Subset to recent + oldest survey
# dhs_all_df <- dhs_all_df %>%
#   dplyr::filter(most_recent_survey | oldest_survey)

# Cleanup wealth index ---------------------------------------------------------
dhs_all_df <- dhs_all_df %>%
  
  # Wealth Index Category
  dplyr::mutate(wealth_index = wealth_index %>% as.numeric()) %>%

  # Standardize Wealth Score
  # Some countries had wealth score scaled differently; rescale so that, for all
  # country-years, ranged from 1-5.
  ungroup() %>%
  group_by(country_code, year) %>%
  dplyr::mutate(wealth_index_score = scales::rescale(wealth_index_score, 
                                                     to = c(1,5), 
                                                     na.rm=T)) %>%
  ungroup()

# Deal with missing/don't know codes -------------------------------------------
# e.g., value of 99 that means "don't know" -- these should be NA

#### Time to get water
# 996; at source --> 0
# 998/999; don't know -->NA
dhs_all_df$water_time_to_get[dhs_all_df$water_time_to_get %in% 996] <- 0
dhs_all_df$water_time_to_get[dhs_all_df$water_time_to_get %in% c(998,999)] <- NA

#### Assets
dhs_all_df$has_car[dhs_all_df$has_car %in% 9] <- NA
dhs_all_df$has_electricity[dhs_all_df$has_electricity %in% 9] <- NA
dhs_all_df$has_fridge[dhs_all_df$has_fridge %in% 9] <- NA
dhs_all_df$has_motorbike[dhs_all_df$has_motorbike %in% 9] <- NA
dhs_all_df$has_radio[dhs_all_df$has_radio %in% 9] <- NA
dhs_all_df$has_tv[dhs_all_df$has_tv %in% 9] <- NA

# Fix issues with specific surveys ---------------------------------------------
# In Tanzania, wall & roof variables coded differently
dhs_all_df$wall_material[(dhs_all_df$country_code %in% "TZ" & dhs_all_df$year %in% 2010)] <- NA
dhs_all_df$roof_material[(dhs_all_df$country_code %in% "TZ" & dhs_all_df$year %in% 2010)] <- NA

# Prep variables for asset index -------------------------------------------------
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

# Make variables numeric -------------------------------------------------------
# Some variables labelled double, which causes issues

dhs_all_df <- dhs_all_df %>%
  mutate_at(pca_allvars, as.numeric)

pca_allvars <- c("has_electricity",
                 "has_radio",
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
                 "educ_years_hh_max_scale",
                 "n_sleeping_rooms_pp_cat")

# dhs_all_df %>%
#   ungroup() %>%
#   dplyr::select_at(vars(pca_allvars)) %>%
#   dplyr::summarise_all( . %>% min(na.rm = T)) %>%
#   t %>%
#   as.data.frame()
# 
# dhs_all_df %>%
#   ungroup() %>%
#   dplyr::select_at(vars(pca_allvars)) %>%
#   dplyr::summarise_all( . %>% max(na.rm = T)) %>%
#   t %>%
#   as.data.frame() 
# 
# dhs_all_df_sub <- dhs_all_df %>%
#   dplyr::filter(year < 2000,
#                 !is.na(has_radio))
# 
# N <- nrow(dhs_all_df_sub)
# dhs_all_df_sub %>%
#   ungroup() %>%
#   dplyr::select_at(vars(pca_allvars)) %>%
#   dplyr::summarise_all( . %>% is.na %>% sum) %>%
#   t %>%
#   as.data.frame() %>%
#   dplyr::mutate(prop = V1/N)

# Variable sets for PCA --------------------------------------------------------

# PCA - constant over time
pca_allvars_alltime <- c("has_electricity",
                         "has_radio",
                         "has_tv",
                         "has_fridge",
                         "has_motorbike", # a lot missing
                         "has_car", # a lot missing
                         "floor_material_cat",
                         "water_source_piped_dwelling",
                         "educ_years_hh_max_scale",
                         "flush_toilet_sewer")

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
                 "educ_years_hh_max_scale",
                 "n_sleeping_rooms_pp_cat")

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
                        "educ_years_hh_max_scale",
                        "n_sleeping_rooms_pp_cat")

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
                         "educ_years_hh_max_scale",
                         "n_sleeping_rooms_pp_cat")

# Compute PCAs -----------------------------------------------------------------
dhs_all_df$ind_id <- 1:nrow(dhs_all_df)

compute_pca_impute_missing <- function(pca_vars, dhs_all_df){
  
  pca_df <- dhs_all_df %>%
    dplyr::select(all_of(pca_vars)) %>%
    mutate_all(as.numeric) %>%
    mutate_all(. %>% scales::rescale(to = c(0,1)))
  
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
    drop_na() %>%
    mutate_at(vars(-ind_id),
              . %>% scales::rescale(to = c(0,1)))
  
  pca_obj <- pca_df %>%
    dplyr::select(-ind_id) %>%
    prcomp(scale = T)
  
  out_df <- data.frame(pca = pca_obj$x[,1]*(-1),
                       ind_id = pca_df$ind_id)
  
  names(out_df)[1] <- var_name
  
  return(out_df)
}

#### Most Recent Data

## PCA - Removing NAs
# dhs_all_df <- dhs_all_df %>%
#   left_join(compute_pca_rm_na(pca_allvars, "pca_allvars_rmna", dhs_all_df), 
#             by = "ind_id") %>%
#   left_join(compute_pca_rm_na(pca_allvars_noroof, "pca_allvars_noroof_rmna", dhs_all_df), 
#             by = "ind_id") %>%
#   left_join(compute_pca_rm_na(pca_physicalvars, "pca_physicalvars_rmna", dhs_all_df), 
#             by = "ind_id") %>%
#   left_join(compute_pca_rm_na(pca_physicalvars_noroof, "pca_physicalvars_noroof_rmna", dhs_all_df), 
#             by = "ind_id") %>%
#   left_join(compute_pca_rm_na(pca_nonphysicalvars, "pca_nonphysicalvars_rmna", dhs_all_df), 
#             by = "ind_id")

#### PCA - Imputing Missing NAs
# Subset to most recent
dhs_all_df_mr <- dhs_all_df %>%
  dplyr::filter(most_recent_survey %in% T)

# Compute PCA
dhs_all_df_mr$pca_allvars_mr             <- compute_pca_impute_missing(pca_allvars, dhs_all_df_mr)
dhs_all_df_mr$pca_allvars_noroof_mr      <- compute_pca_impute_missing(pca_allvars_noroof, dhs_all_df_mr)
dhs_all_df_mr$pca_physicalvars_mr        <- compute_pca_impute_missing(pca_physicalvars, dhs_all_df_mr)
dhs_all_df_mr$pca_physicalvars_noroof_mr <- compute_pca_impute_missing(pca_physicalvars_noroof, dhs_all_df_mr)
dhs_all_df_mr$pca_nonphysicalvars_mr     <- compute_pca_impute_missing(pca_nonphysicalvars, dhs_all_df_mr)

# Merge PCA variables back into main survey
dhs_all_df_mr <- dhs_all_df_mr %>%
  dplyr::select(ind_id,
                pca_allvars_mr,
                pca_allvars_noroof_mr,
                pca_physicalvars_mr,
                pca_physicalvars_noroof_mr,
                pca_nonphysicalvars_mr)

dhs_all_df <- dhs_all_df %>%
  left_join(dhs_all_df_mr, by = "ind_id")

#### All Data
dhs_all_df$pca_allvars <- compute_pca_impute_missing(pca_allvars_alltime, dhs_all_df)

# Construct additional variables -----------------------------------------------
dhs_all_df <- dhs_all_df %>%
  dplyr::mutate(educ_levels_hh_max_0 = as.numeric(educ_levels_hh_max == 0),
                educ_levels_hh_max_1 = as.numeric(educ_levels_hh_max == 1),
                educ_levels_hh_max_2 = as.numeric(educ_levels_hh_max == 2),
                educ_levels_hh_max_3 = as.numeric(educ_levels_hh_max == 3),
                educ_levels_hh_max_23 = as.numeric(educ_levels_hh_max == 2 | educ_levels_hh_max == 3))

# Aggregate --------------------------------------------------------------------
# For some continuous DHS, uid repeats; adding year makes unique
dhs_all_df <- dhs_all_df %>%
  dplyr::mutate(uid = case_when(
    (country_code %in% "PE") & (year %in% c(2004, 2007)) ~ paste0(uid, year),
    TRUE ~ uid
  ))

## Averages
dhs_all_df_coll <- dhs_all_df %>%
  group_by(uid, country_code, country_year, urban_rural, year, most_recent_survey, oldest_survey) %>%
  summarise_if(is.numeric, mean, na.rm=T) %>%
  ungroup()

## Sum
dhs_all_df_coll_sum <- dhs_all_df %>%
  group_by(uid) %>%
  summarise_at(vars(n_hh_members,
                    educ_levels_hh_n0,
                    educ_levels_hh_n1,
                    educ_levels_hh_n2,
                    educ_levels_hh_n3,
                    educ_levels_hh_n3g), sum, na.rm=T) %>%
  ungroup() %>%
  rename_with(~paste0(., "_sum"), -c("uid"))

## Standard Deviation
dhs_all_df_coll_stddev <- dhs_all_df %>%
  group_by(uid) %>%
  summarise_at(vars(pca_allvars, pca_allvars_mr), sd, na.rm=T) %>%
  ungroup() %>%
  rename_with(~paste0(., "_stddev"), -c("uid"))

dhs_all_df_coll <- dhs_all_df_coll %>%
  left_join(dhs_all_df_coll_sum, by = "uid") %>%
  left_join(dhs_all_df_coll_stddev, by = "uid")

# Country Name -----------------------------------------------------------------
dhs_all_df_coll$iso2 <- countrycode(dhs_all_df_coll$country_code, origin = "dhs", destination = "iso2c")
dhs_all_df_coll$country_name <- countrycode(dhs_all_df_coll$iso2, origin = "iso2c", destination = "country.name")

dhs_all_df_coll$country_code %>% unique %>% length()
dhs_all_df_coll$iso2 %>% unique %>% length()
dhs_all_df_coll$country_name %>% unique %>% length()

# Cleanup Variables ------------------------------------------------------------
dhs_all_df_coll <- dhs_all_df_coll %>%
  dplyr::mutate(uid = uid %>% as.character())

# Export -----------------------------------------------------------------------
saveRDS(dhs_all_df_coll, file.path(dhs_exp_dir, "FinalData", "Individual Datasets", 
                                   "survey_socioeconomic_varconstructed_tmp.Rds"))


