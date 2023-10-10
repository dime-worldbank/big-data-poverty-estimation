# Clean LSMS

# Load data --------------------------------------------------------------------
lsms_df <- read_dta(file.path(lsms_dir, "RawData", "clean_East_west_africa.dta"))

# Append -----------------------------------------------------------------------
lsms_df <- lsms_df %>%
  dplyr::select(HHID,
                country,
                LAT,
                LON,
                year,
                has_electricity,
                has_radio,
                has_tv,
                has_fridge,
                has_motorbike,
                has_car,
                floor_material_cat,
                wall_material_cat,
                roof_material_cat,
                water_time_to_get_cat,
                water_source_piped_dwelling,
                flush_toilet_sewer,
                n_sleeping_rooms_pp_cat,
                poverty_measure)

lsms_df <- lsms_df %>%
  dplyr::filter(!is.na(LAT),
                !is.na(LON)) %>%
  dplyr::filter(LAT != 0,
                LON != 0) %>%
  dplyr::filter(!is.na(poverty_measure),
                !is.na(year))

# Compute PCA ------------------------------------------------------------------
compute_pca_impute_missing <- function(pca_vars, 
                                       dhs_all_df,
                                       save_model = F,
                                       model_path = NULL){
  
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
  out <- res.pca$x[,1]#*(-1)
  
  if(save_model){
    saveRDS(res.pca, model_path)
  }
  
  return(out)
}

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
                 "n_sleeping_rooms_pp_cat")

lsms_df$pca_allvars_mr <- compute_pca_impute_missing(pca_allvars, lsms_df, T, "~/Desktop/lsms_pca.Rds")

lsms_df %>%
  group_by(country) %>%
  dplyr::summarise_at(vars(all_of(pca_allvars)), mean, na.rm = T) %>%
  t()

lsms_df$has_motorbike[lsms_df$country == "BFA"] %>% table()

# Aggregate --------------------------------------------------------------------
cluster_df <- lsms_df %>%
  group_by(country, LAT, LON, year) %>%
  dplyr::summarise(pca_allvars_mr  = mean(pca_allvars_mr, na.rm = T),
                   poverty_measure = mean(poverty_measure, na.rm = T)) %>%
  ungroup()

# Cleanup ----------------------------------------------------------------------
cluster_df <- cluster_df %>%
  mutate(uid = paste(country, LAT, LON, year) %>% as.factor() %>% as.numeric(),
         uid = paste0("id_", uid)) %>%
  dplyr::mutate(iso2 = case_when(
    country == "BEN" ~ "BJ",
    country == "BFA" ~ "BF",
    country == "CIV" ~ "CI",
    country == "ETH" ~ "ET",
    country == "MWI" ~ "MW",
    country == "TGO" ~ "TG"
  ),
  country_code = iso2) %>%
  dplyr::rename(latitude = LAT,
                longitude = LON) %>%
  mutate(most_recent_survey = T)

# Export -----------------------------------------------------------------------
saveRDS(cluster_df, file.path(lsms_dir, "FinalData", "Individual Datasets", 
                              "survey_socioeconomic_varconstructed_tmp.Rds"))




