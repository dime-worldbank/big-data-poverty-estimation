# Clean OPM Data

# Clean Oxford Policy Management (OPM) survey data. Create Household Level 
# dataframe with relevant socioeconomic variables.

# Load Data --------------------------------------------------------------------
opm_df <- readRDS(file.path(secure_file_path, "Data", "OPM", "FinalData - PII", "opm_socioeconomic_geo.Rds"))

opm_df <- opm_df %>%
  dplyr::filter(!is.na(latitude))

opm_df$loc <- paste(opm_df$psu, opm_df$province, opm_df$locality)

cluster_df <- opm_df %>%
  group_by(year, psu) %>%
  dplyr::summarise(pscores_mean = mean(pscores),
                   pscores_min = min(pscores),
                   pscores_max = max(pscores),
                   pscores_sd = sd(pscores),
                   N = n())

a <- cluster_df[cluster_df$year %in% 2014,]
a$pscores_sd %>% hist()





leaflet() %>%
  addTiles() %>%
  addCircles(data = opm_df[opm_df$psu %in% 100 & opm_df$year %in% 2011,],
             lat = ~latitude,
             lng = ~longitude,
             popup = ~paste0(opm_df$pscores))


opm_df$pscores %>% hist()



opm_df$psu





