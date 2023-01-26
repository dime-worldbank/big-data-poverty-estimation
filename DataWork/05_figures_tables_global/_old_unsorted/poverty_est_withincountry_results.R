# Facebook Correlation

## Survey
survey_df <- readRDS(file.path(dhs_dir, "FinalData", "Individual Datasets", "survey_socioeconomic.Rds"))

survey_sum_df <- survey_df %>%
  dplyr::group_by(country_code) %>%
  dplyr::summarise(survey_year = min(year),
                   survey_N = n())

# Load Data --------------------------------------------------------------------
results_df <- readRDS(file.path(data_dir, SURVEY_NAME, "FinalData", "results", "results.Rds"))
y_df <- readRDS(file.path(data_dir, SURVEY_NAME, "FinalData", "results", "predicted_values.Rds"))

y_df <- y_df %>%
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

results_df <- results_df %>%
  dplyr::mutate(country_name = case_when(
    country == "BD" ~ "Bangladesh",
    country == "IA" ~ "India",
    country == "KH" ~ "Cambodia",
    country == "KY" ~ "Kyrgyzstan",
    country == "MM" ~ "Myanmar",
    country == "NP" ~ "Nepal",
    country == "PH" ~ "Philippines",
    country == "PK" ~ "Pakistan",
    country == "TJ" ~ "Tajikistan",
    country == "TL" ~ "Timor Leste"
  ))

rsq <- function (x, y) cor(x, y) ^ 2

y_long_df <- y_df %>%
  pivot_longer(cols = -c(uid, country_name, country_code, y, target, feature_type)) %>%
  dplyr::mutate(param_id = name %>% str_replace_all("y_", "")) 

r2_df <- y_long_df %>%
  dplyr::group_by(country_name, country_code, param_id, target, feature_type) %>%
  dplyr::summarise(r2 = rsq(y, value))

r2_max_df <- r2_df %>%
  dplyr::ungroup() %>%
  arrange(desc(r2)) %>%
  distinct(country_name, country_code, target, feature_type, .keep_all = T) %>%
  mutate(best_model = 1)

y_best_df <- y_long_df %>%
  right_join(r2_max_df, by = c("country_code", "country_name", "param_id", "target", "feature_type"))

# Table ------------------------------------------------------------------------
r2_max_df_t <- r2_max_df %>%
  dplyr::select(-param_id) %>%
  pivot_wider(names_from = feature_type,
              values_from = r2) %>%
  left_join(survey_sum_df, by = "country_code") %>%
  dplyr::filter(target %in% "wealth_index_score") %>%
  arrange(country_name) %>%
  dplyr::mutate(tex = paste(country_name, "&", survey_year, "&", survey_N, "&", 
                            round(l8, 2), "&",
                            round(l8_viirs, 2), "&",
                            round(osm, 2), "&",
                            round(fb, 2), "&",
                            round(all, 2), "\\\\ \n"))

sink(file.path(tables_dir, "fb_withincv_wealth_index_score.tex"))
cat("\\begin{tabular}{lcc | ccccc} \n")
cat("\\hline \n")
cat("        & Survey & Survey  & \\multicolumn{5}{c}{R$^2$ Across Diff. Feature Sets} \\\\ \n")
cat("Country &  Year & N        & DTL & DTL and NTL  & OSM & FB & All \\\\ \n")
cat("\\hline \n")
for(i in 1:nrow(r2_max_df_t)) cat(r2_max_df_t$tex[i])
cat("\\hline \n")
cat("\\end{tabular} \n")
sink()

# Figure -----------------------------------------------------------------------
y_best_df <- y_best_df %>%
  left_join(survey_df %>%
              dplyr::select(uid, urban_rural), by = "uid") %>%
  dplyr::mutate(urban_rural = case_when(
    urban_rural %in% "U" ~ "Urban",
    urban_rural %in% "R" ~ "Rural"
  )) %>%
  dplyr::mutate(country_name = paste0(country_name, "\nR^2 = ", round(r2, 2)))

p <- y_best_df %>%
  dplyr::filter(target %in% "wealth_index_score",
                feature_type %in% "all") %>% 
  dplyr::mutate(y = y / 100000,
                value = value / 100000) %>%
  ggplot() +
  geom_point(aes(x = y,
                 y = value,
                 color = urban_rural),
             size = 0.2,
             alpha = 0.5) +
  theme_minimal() +
  theme(strip.text = element_text(face = "bold")) +
  labs(x = "True Poverty Score",
       y = "Predicted Poverty Score",
       color = NULL) +
  scale_color_manual(values = c("orange2", "dodgerblue3")) +
  facet_wrap(~country_name,
             scales = "free",
             nrow = 2)
ggsave(p, filename = file.path(figures_dir, "fb_withincv_wealth_index_score.png"),
       height = 6, width = 12)





