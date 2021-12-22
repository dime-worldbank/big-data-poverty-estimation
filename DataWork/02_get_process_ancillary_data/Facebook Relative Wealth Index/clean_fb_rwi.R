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
  })

# Add iso2 ---------------------------------------------------------------------
fb_rwi_df$iso2s <- countrycode(fb_rwi_df$iso3s,
                               origin = "iso3c",
                               destination = "iso2c")

fb_rwi_df <- fb_rwi_df %>%
  dplyr::mutate(iso2s = case_when(
    iso3s %in% "IND_PAK" ~ "IN_PK",
    TRUE ~ iso2s
  ))

# Export data ------------------------------------------------------------------
saveRDS(fb_rwi_df, file.path(fb_rwi_dir, "FinalData", "fb_rwi.Rds"))
