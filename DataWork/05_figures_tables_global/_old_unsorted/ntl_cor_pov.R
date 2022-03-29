# Facebook Correlation

SURVEY_NAME <- "DHS"

# Load Data --------------------------------------------------------------------
survey_df <- readRDS(file.path(dhs_dir, "FinalData", "Individual Datasets", "survey_socioeconomic.Rds"))
l8_df <- read.csv(file.path(dhs_dir, "FinalData", "Individual Datasets", "survey_l8.csv"))
viirs_df <- read.csv(file.path(dhs_dir, "FinalData", "Individual Datasets", "survey_viirs.csv"))

survey_df <- survey_df %>%
  left_join(viirs_df, by = "uid") %>%
  left_join(l8_df, by = "uid") %>%
  dplyr::mutate(viirs_avg_rad = log(viirs_avg_rad + 1))

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

survey_df <- survey_df %>% 
  dplyr::group_by(country_name) %>%
  dplyr::mutate(wealth_viirs = cor(wealth_index_score, viirs_avg_rad)) %>%
  ungroup() %>%
  mutate(wealth_viirs = round(wealth_viirs, 2)) %>%
  mutate(country_name = paste0(country_name, "\nCor: ", wealth_viirs))

p <- survey_df %>%
  dplyr::mutate(urban_rural = case_when(
    urban_rural == "U" ~ "Urban",
    urban_rural == "R" ~ "Rural"
  )) %>%
  dplyr::mutate(wealth_index_score = wealth_index_score / 100000) %>%
  ggplot() + 
  geom_point(aes(x = wealth_index_score,
                 y = viirs_avg_rad,
                 color = urban_rural),
             alpha = 0.5,
             size = 0.2) +
  facet_wrap(~country_name,
             scales = "free",
             nrow = 2) +
  labs(x = "Poverty Score",
       y = "NTL: log(VIIRS)",
       color = NULL) +
  scale_color_manual(values = c("orange2",
                                "dodgerblue3")) +
  theme_minimal() +
  theme(strip.text = element_text(face = "bold"))
ggsave(p, filename = file.path(figures_file_path, "dhs_pov_ntl.png"),
       height = 6, width = 12)

