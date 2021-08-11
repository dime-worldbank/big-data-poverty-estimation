# Facebook Correlation

## Survey
survey_df <- readRDS(file.path(dhs_dir, "FinalData", "Individual Datasets", "survey_socioeconomic.Rds"))

survey_sum_df <- survey_df %>%
  dplyr::group_by(country_code) %>%
  dplyr::summarise(survey_year = min(year),
                   survey_N = n())

# Load Data --------------------------------------------------------------------
y_df <- read.csv(file.path(dhs_dir, "FinalData", "results", "ypred_fbonly_withincv.csv"), stringsAsFactors = F)
results_df <- read.csv(file.path(dhs_dir, "FinalData", "results", "results_fbonly_withincv.csv"), stringsAsFactors = F)
#a <- results_df[results_df$regressor %in% "BaggingRegressor",]

# y_df <- bind_rows(
#   y_df %>% mutate(feature_type = "fb_only"),
#   y_df %>% mutate(feature_type = "sat_only"),
#   y_df %>% mutate(feature_type = "all")
# )
# 
# results_df <- bind_rows(
#   results_df %>% mutate(feature_type = "fb_only"),
#   results_df %>% mutate(feature_type = "sat_only"),
#   results_df %>% mutate(feature_type = "all")
# )

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
  dplyr::select(-X) %>%
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
                            round(sat_only, 2), "&",
                            round(fb_only, 2), "&",
                            round(all, 2), "\\\\ \n"))

sink(file.path(tables_file_path, "fb_withincv_wealth_index_score.tex"))
cat("\\begin{tabular}{lcc | ccc} \n")
cat("\\hline \n")
cat("        & Survey & Survey  & \\multicolumn{3}{c}{R$^2$ Across Diff. Feature Sets} \\\\ \n")
cat("Country &  Year & N        & Sat. Only & FB Only & All \\\\ \n")
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
  ))

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
ggsave(p, filename = file.path(figures_file_path, "fb_withincv_wealth_index_score.png"),
       height = 6, width = 12)





