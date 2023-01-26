# Alluvial Diagram of Poverty Over Time

# Load Data --------------------------------------------------------------------
bisp_df <- readRDS(file.path(secure_file_path, "Data", "OPM", "FinalData - PII", "opm_socioeconomic_geo.Rds"))



p <- bisp_df %>%
  dplyr::filter(!is.na(latitude)) %>%
  ggplot() +
  geom_histogram(aes(x = pscores),
                 fill = "dodgerblue3") +
  labs(x = "Poverty Score",
       y = "N Households") + 
  theme_minimal() +
  theme(strip.text = element_text(face = "bold")) +
  facet_wrap(~year) 
ggsave(p, filename = file.path(figures_file_path, "opm_pscore_dist.png"),
       height = 3.5, width = 5)

bisp_df <- bisp_df %>%
  dplyr::filter(!is.na(latitude))

paste(bisp_df$latitude,
      bisp_df$longitude) %>% table %>% table
