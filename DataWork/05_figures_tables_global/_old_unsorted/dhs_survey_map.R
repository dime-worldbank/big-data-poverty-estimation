# Map of Survey Locations

# Load data --------------------------------------------------------------------
survey_df <- readRDS(file.path(dhs_dir, "FinalData", "Individual Datasets", "survey_socioeconomic.Rds"))

# Figure -----------------------------------------------------------------------
#### Basemap
shift <- 1
basemap <- get_stamenmap(bbox = c(left = min(survey_df$longitude) - shift,
                                  bottom = min(survey_df$latitude) - shift,
                                  right = max(survey_df$longitude) + shift,
                                  top = max(survey_df$latitude) + shift),
                         maptype = "toner-lite", 
                         crop = FALSE,
                         zoom = 4)

#### Figure
p <- ggmap(basemap) +
  geom_point(data = survey_df,
             aes(x = longitude,
                 y = latitude,
                 color = country_name),
             size = 0.2) +
  theme_void() +
  labs(color = "Country")
ggsave(p, filename = file.path(figures_dir, "dhs_map.png"),
       height = 6, width = 10)




