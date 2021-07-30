# Facebook Correlation

## Survey
survey_df <- readRDS(file.path(dhs_dir, "FinalData", "Individual Datasets", "survey_socioeconomic.Rds"))

survey_df <- survey_df %>%
  dplyr::mutate(country_name = case_when(
    country_code == "BD" ~ "Bangladesh",
    country_code == "IA" ~ "India",
    country_code == "KH" ~ "Cambodia",
    country_code == "KY" ~ "Kyrgyzstan",
    country_code == "MM" ~ "Myanmar",
    country_code == "NP" ~ "Nepal",
    country_code == "PH" ~ "Philippines",
    country_code == "PK" ~ "Pakistan",
    country_code == "TJ" ~ "Tajikistan",
    country_code == "TL" ~ "Timor Leste"
  ))


shift <- 1
basemap <- get_stamenmap(bbox = c(left = min(survey_df$longitude) - shift,
                                  bottom = min(survey_df$latitude) - shift,
                                  right = max(survey_df$longitude) + shift,
                                  top = max(survey_df$latitude) + shift),
                         maptype = "toner-lite", 
                         crop = FALSE,
                         zoom = 5)

p <- ggmap(basemap) +
  geom_point(data = survey_df,
             aes(x = longitude,
                 y = latitude,
                 color = country_name),
             size = 0.2) +
  theme_void() +
  labs(color = "Country")
ggsave(p, filename = file.path(figures_file_path, "dhs_map.png"),
       height = 6, width = 10)
