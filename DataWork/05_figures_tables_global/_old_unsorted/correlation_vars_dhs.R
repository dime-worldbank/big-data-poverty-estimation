# Correlation Across Variables

# Load Data --------------------------------------------------------------------
df <- readRDS(file.path(data_dir, "DHS", "FinalData", "Merged Datasets", "survey_alldata_clean.Rds"))

dfa <- df %>%
  dplyr::group_by(country_code) %>%
  dplyr::summarise(a = mean(fb_estimate_mau_1 %in% 1000))

df_cor_country <- df %>%
  dplyr::select_at(vars(country_name, 
                        continent_adj,
                        pca_allvars,
                        contains("osm_"),
                        contains("worldpop_"),
                        contains("l8_"),
                        contains("elevslope_"),
                        contains("globalmod_"),
                        contains("pollution_"),
                        contains("gc_"),
                        contains("viirs_"))) %>%
  pivot_longer(cols = -c(country_name, continent_adj, pca_allvars)) %>%
  group_by(name, country_name, continent_adj) %>%
  dplyr::summarise(cor = as.numeric(cor.test(pca_allvars, value)$estimate)) %>%
  mutate(var_category = name %>% str_replace_all("_.*", "")) %>%
  mutate(r2 = cor^2) %>%
  mutate(var_category = case_when(
    # name %>% str_detect("osm_distmeters_poi_") ~ "OSM: Dist POI",
    # name %>% str_detect("osm_n_poi") ~ "OSM: N POI",
    # name %>% str_detect("osm_dist_") ~ "OSM: Dist Road",
    # name %>% str_detect("osm_N_segments_") ~ "OSM: N Road Segments",
    # name %>% str_detect("osm_length_") ~ "OSM: Road Length",
    # name %>% str_detect("osm_N_segments_") ~ "OSM: N Road Segments",
    var_category == "osm" ~ "OSM",
    var_category == "gc" ~ "Globcover",
    var_category == "viirs" ~ "NTL: VIIRS",
    TRUE ~ var_category
  ))

df_cor_country <- df_cor_country %>%
  group_by(name) %>%
  dplyr::mutate(r2_median = median(r2, na.rm=T)) %>%
  ungroup() %>%
  mutate(name = name %>% as.factor())

df_cor_avg <- df_cor_country %>%
  distinct(name, var_category, r2_median) %>%
  arrange(desc(r2_median)) %>%
  group_by(var_category) %>%
  mutate(category_rank = 1:n()) %>%
  ungroup()

df_cor_avg <- df_cor_avg %>%
  dplyr::filter(category_rank <= 5)

df_cor_country %>%
  dplyr::filter(!is.na(r2)) %>%
  dplyr::filter(name %in% df_cor_avg$name) %>%
  ggplot(aes(x = r2,
             y = reorder(name, r2, median),
             fill = var_category)) +
  geom_density_ridges(quantile_lines = TRUE, 
                      quantiles = c(0.5), 
                      alpha = 0.7,
                      jittered_points = F,
                      point_color = "red",
                      point_size = 0.2) +
  theme_minimal() +
  labs(fill = "Variable\nCategory")





