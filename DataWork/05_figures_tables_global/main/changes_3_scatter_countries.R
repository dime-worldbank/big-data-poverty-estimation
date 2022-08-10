# Changes: Main Figure

FILL_COLOR <- "gray80" 

# For adding text onto boxplot
p75 <- function(x) quantile(x, probs = 0.75) %>% as.numeric()

# Load/prep survey data --------------------------------------------------------
cluster_df <- readRDS(file.path(data_dir, SURVEY_NAME, "FinalData", "Merged Datasets",
                                "survey_alldata_clean_changes_cluster_predictions.Rds"))

cluster_df <- cluster_df %>%
  dplyr::mutate(country_name = case_when(
    country_name == "Congo - Kinshasa" ~ "Congo - DRC",
    TRUE ~ country_name
  ))

# TODO: What's going on with Liberia?
cluster_df$predict_pca_allvars_best[cluster_df$country_code %in% "LB"] <- cluster_df$predict_pca_allvars_best[cluster_df$country_code %in% "LB"] / 100

district_df <- cluster_df %>%
  group_by(continent_adj, country_code, country_name, gadm_uid) %>%
  summarise_if(is.numeric, mean) %>%
  ungroup()

## Add correlation
cluster_df <- cluster_df %>%
  group_by(country_name) %>%
  mutate(r2 = cor(pca_allvars, predict_pca_allvars_best)^2,
         r2_round = round(r2, 2) %>% as.character() %>% paste0("r2 = ",.),
         r2_round = case_when(
           r2_round == "R2=0" ~ "R2<0.01",
           TRUE ~ r2_round
         )) %>%
  ungroup() %>%
  mutate(country_name_r2 = paste0(country_name, "\n", r2_round)) %>%
  dplyr::mutate(country_name = fct_reorder(country_name, -r2),
                country_name_r2 = fct_reorder(country_name_r2, -r2)) 

district_df <- district_df %>%
  group_by(country_name) %>%
  mutate(r2 = cor(pca_allvars, predict_pca_allvars_best)^2,
         r2_round = round(r2, 2) %>% as.character() %>% paste0("r2 = ",.),
         r2_round = case_when(
           r2_round == "R2=0" ~ "R2<0.01",
           TRUE ~ r2_round
         )) %>%
  ungroup() %>%
  mutate(country_name_r2 = paste0(country_name, "\n", r2_round)) %>%
  dplyr::mutate(country_name = fct_reorder(country_name, -r2),
                country_name_r2 = fct_reorder(country_name_r2, -r2)) 

# All countries ----------------------------------------------------------------
p_scatter_cluster <- cluster_df %>%
  ggplot(aes(x = pca_allvars,
             y = predict_pca_allvars_best)) +
  geom_point(size = 0.5) +
  #stat_poly_eq(color = "red") +
  labs(x = "True Change in Asset Index",
       y = "Estimated Change in\nAsset Index",
       #title = "A. Cluster Level",
       color = NULL) +
  theme_classic() +
  theme(strip.background = element_blank(),
        strip.text = element_text(face = "bold",size = 10),
        plot.title = element_text(face = "bold")) +
  facet_wrap(~country_name_r2,
             scales = "free") 

p_scatter_district <- district_df %>%
  ggplot(aes(x = pca_allvars,
             y = predict_pca_allvars_best)) +
  geom_point(size = 0.75) +
  #stat_poly_eq(color = "red") +
  labs(x = "True Change in Asset Index",
       y = "Estimated Change in\nAsset Index",
       #title = "B. District Level",
       color = NULL) +
  theme_classic() +
  theme(strip.background = element_blank(),
        strip.text = element_text(face = "bold",size = 10),
        plot.title = element_text(face = "bold")) +
  facet_wrap(~country_name_r2,
             scales = "free")

ggsave(p_scatter_cluster,
       filename = file.path(figures_global_dir, "changes_scatter_cluster.png"),
       height = 10,
       width = 12)

ggsave(p_scatter_district,
       filename = file.path(figures_global_dir, "changes_scatter_adm2.png"),
       height = 10,
       width = 12)

# Example countries ------------------------------------------------------------
## Top X countries
cluster_countries <- cluster_df %>%
  distinct(country_name, .keep_all = T) %>%
  arrange(-r2) %>%
  head(7) %>%
  pull(country_name)

district_countries <- district_df %>%
  distinct(country_name, .keep_all = T) %>%
  arrange(-r2) %>%
  head(7) %>%
  pull(country_name)

## Figures
p_scatter_cluster_ex <- cluster_df %>%
  filter(country_name %in% cluster_countries) %>%
  ggplot(aes(x = pca_allvars,
             y = predict_pca_allvars_best)) +
  geom_point(size = 1) +
  stat_poly_eq(color = "red", small.r=T) +
  labs(x = "True Change in Asset Index",
       y = "Estimated Change in\nAsset Index",
       title = "A. Cluster Level",
       color = NULL) +
  theme_classic() +
  theme(strip.background = element_blank(),
        strip.text = element_text(face = "bold",size = 11),
        plot.title = element_text(face = "bold")) +
  facet_wrap(~country_name,
             scales = "free",
             nrow = 1) 

p_scatter_district_ex <- district_df %>%
  filter(country_name %in% district_countries) %>%
  ggplot(aes(x = pca_allvars,
             y = predict_pca_allvars_best)) +
  geom_point(size = 1) +
  stat_poly_eq(color = "red", small.r=T) +
  labs(x = "True Change in Asset Index",
       y = "Estimated Change in\nAsset Index",
       title = "B. ADM2 Level",
       color = NULL) +
  theme_classic() +
  theme(strip.background = element_blank(),
        strip.text = element_text(face = "bold",size = 11),
        plot.title = element_text(face = "bold")) +
  facet_wrap(~country_name,
             scales = "free",
             nrow = 1)

p <- ggarrange(p_scatter_cluster_ex,
               p_scatter_district_ex,
               ncol = 1)

ggsave(p,
       filename = file.path(figures_global_dir, "changes_scatter_examples.png"),
       height = 5,
       width = 12)


