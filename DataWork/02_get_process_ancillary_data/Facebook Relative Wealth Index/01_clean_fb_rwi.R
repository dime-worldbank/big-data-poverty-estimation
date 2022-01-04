# Clean Facebook RWI Data

# Append data. Raw files split across countries; here append into one dataset

# Load data --------------------------------------------------------------------
fb_rwi_df <- file.path(fb_rwi_dir, "RawData", "relative-wealth-index-april-2021") %>%
  list.files(pattern = "*.csv",
             full.names = T) %>%
  map_df(function(file_i){
    
    df <- fread(file_i)
    
    df$iso3s <- file_i %>%
      str_replace_all(".*/", "") %>%
      str_replace_all("_relative_wealth_index.csv", "")
    
    return(df)
  }) %>%
  dplyr::mutate(quadkey = paste0("fbrwi", quadkey)) %>%
  dplyr::rename(uid = quadkey) %>%
  as.data.frame()

# Determine iso3 for India and Pakistan ----------------------------------------
# Facebook RWI dataset combines data for India and Pakistan, so iso3 is
# IND_PAK; separate out into respective countries.

pak_adm <- readRDS(file.path(gadm_dir, "RawData", "gadm36_PAK_0_sp.rds"))
ind_adm <- readRDS(file.path(gadm_dir, "RawData", "gadm36_IND_0_sp.rds"))

fb_rwi_df_indpak <- fb_rwi_df[fb_rwi_df$iso3s %in% "IND_PAK",]
fb_rwi_df_not_indpak <- fb_rwi_df[!(fb_rwi_df$iso3s %in% "IND_PAK"),]

fb_rwi_df_indpak_sp <- fb_rwi_df_indpak
coordinates(fb_rwi_df_indpak_sp) <- ~longitude+latitude
crs(fb_rwi_df_indpak_sp) <- CRS("+init=epsg:4326")

## Check intersection
fb_OVER_pak <- over_chunks(fb_rwi_df_indpak_sp, pak_adm, fn_type = "none", chunk_size = 2000)
fb_OVER_ind <- over_chunks(fb_rwi_df_indpak_sp, ind_adm, fn_type = "none", chunk_size = 600)

fb_rwi_df_indpak_sp$in_pak <- fb_OVER_pak$NAME_0 %in% "Pakistan"
fb_rwi_df_indpak_sp$in_ind <- fb_OVER_ind$NAME_0 %in% "India"

fb_rwi_df_indpak_sp$iso3s[(fb_rwi_df_indpak_sp$in_pak %in% T) & (fb_rwi_df_indpak_sp$in_ind %in% F)] <- "PAK"
fb_rwi_df_indpak_sp$iso3s[(fb_rwi_df_indpak_sp$in_pak %in% F) & (fb_rwi_df_indpak_sp$in_ind %in% T)] <- "IND"

## Separate - (1) Cleanly intersect and (2) Dont cleanly intersect
fb_rwi_df_indpak_inter_sp     <- fb_rwi_df_indpak_sp[!(fb_rwi_df_indpak_sp$iso3s %in% "IND_PAK"),]
fb_rwi_df_indpak_dontinter_sp <- fb_rwi_df_indpak_sp[(fb_rwi_df_indpak_sp$iso3s %in% "IND_PAK"),]

## For ones that don't clearnly intersect, assign based on which is closest
fb_rwi_df_indpak_dontinter_sp$dist_pak <- gDistance_chunks(fb_rwi_df_indpak_dontinter_sp, pak_adm, 100)
fb_rwi_df_indpak_dontinter_sp$dist_ind <- gDistance_chunks(fb_rwi_df_indpak_dontinter_sp, ind_adm, 100)

fb_rwi_df_indpak_dontinter_sp$iso3s <- ifelse(fb_rwi_df_indpak_dontinter_sp$dist_pak < fb_rwi_df_indpak_dontinter_sp$dist_ind, 
                                              "PAK",
                                              "IND")

## Cleanup and append
fb_rwi_df_indpak_dontinter_df <- as.data.frame(fb_rwi_df_indpak_dontinter_sp) %>%
  dplyr::select(uid, latitude, longitude, rwi, error, iso3s)

fb_rwi_df_indpak_inter_df <- as.data.frame(fb_rwi_df_indpak_inter_sp) %>%
  dplyr::select(uid, latitude, longitude, rwi, error, iso3s)

fb_rwi_df <- bind_rows(fb_rwi_df_indpak_inter_df,
                       fb_rwi_df_indpak_dontinter_df,
                       fb_rwi_df_not_indpak)

# Add iso2 ---------------------------------------------------------------------
fb_rwi_df$iso2s <- countrycode(fb_rwi_df$iso3s,
                               origin = "iso3c",
                               destination = "iso2c")

#fb_rwi_df <- fb_rwi_df %>%
#  dplyr::mutate(iso2s = case_when(
#    iso3s %in% "IND_PAK" ~ "IN_PK",
#    TRUE ~ iso2s
#  ))

# Export data ------------------------------------------------------------------
saveRDS(fb_rwi_df, file.path(fb_rwi_dir, "FinalData", "fb_rwi.Rds"))
