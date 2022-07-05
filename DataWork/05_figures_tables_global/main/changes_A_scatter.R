# ML Changes
# Scatterplots

# Load data --------------------------------------------------------------------
survey_df <- readRDS(file.path(data_dir, SURVEY_NAME, "FinalData", "Merged Datasets",
                               "survey_alldata_clean_changes_cluster_predictions.Rds"))

# wdi_df <- readRDS(file.path(data_dir, "WDI", "FinalData", "wdi.Rds"))
# wdi_df <- wdi_df %>%
#   dplyr::select(-c(iso3c, country, year, capital, longitude, latitude))
# 
# survey_df <- survey_df %>%
#   left_join(wdi_df, by = "iso2") 
# 
# survey_df %>%
#   ggplot(aes(x = pca_allvars,
#              y = predict_pca_allvars_within_country_cv_all_changes)) +
#   geom_point() +
#   stat_poly_eq() +
#   facet_wrap(~income)

# Figure -----------------------------------------------------------------------
## Prep
survey_rep_df <- survey_df %>%
  dplyr::mutate(continent_adj = "All") %>%
  bind_rows(survey_df) %>%
  dplyr::mutate(continent_adj = continent_adj %>%
                  factor(levels = c("All",
                                    "Africa", 
                                    "Americas",
                                    "Eurasia")))

## Cluster level
survey_rep_cluster_df <- survey_rep_df %>%
  dplyr::mutate(urban_rural_yr1 = "All") %>%
  bind_rows(survey_rep_df) %>%
  dplyr::mutate(urban_rural_yr1 = case_when(
    urban_rural_yr1 == "R" ~ "Rural",
    urban_rural_yr1 == "U" ~ "Urban",
    TRUE ~ urban_rural_yr1
  ))

p_scatter_cluster <- survey_rep_cluster_df %>%
  ggplot(aes(x = pca_allvars,
             y = predict_pca_allvars_within_country_cv_all_changes,
             color = urban_rural_yr1)) +
  geom_point(size = 0.25) +
  stat_poly_eq() +
  labs(x = "True Asset Index",
       y = "Estimated Asset Index",
       title = "A. Cluster Level",
       color = NULL) +
  scale_color_manual(values = c("black", "chartreuse4", "chocolate2")) +
  theme_classic() +
  theme(strip.background = element_blank(),
        strip.text = element_text(face = "bold",size = 12),
        plot.title = element_text(face = "bold"),
        legend.position = "top") +
  guides(color = guide_legend(override.aes = list(size=1))) +
  facet_wrap(~continent_adj,
             nrow = 1) 

## District level
survey_rep_gadm2_df <- survey_rep_df %>%
  group_by(continent_adj, country_code, gadm_uid) %>%
  summarise_if(is.numeric, mean) %>%
  ungroup()

p_scatter_adm2 <- survey_rep_gadm2_df %>%
  ggplot(aes(x = pca_allvars,
             y = predict_pca_allvars_within_country_cv_all_changes)) +
  geom_point(size = 0.25) +
  stat_poly_eq() +
  labs(x = "True Asset Index",
       y = "Estimated Asset Index",
       title = "B. ADM2 Level") +
  theme_classic() +
  theme(strip.background = element_blank(),
        strip.text = element_text(face = "bold", size = 12),
        plot.title = element_text(face = "bold")) +
  facet_wrap(~continent_adj,
             nrow = 1) 

# Arrange/export ---------------------------------------------------------------
p <- ggarrange(p_scatter_cluster,
               p_scatter_adm2,
               ncol = 1)

ggsave(p,
       filename = file.path(figures_global_dir, "ml_changes_scatter.png"),
       height = 8,
       width = 14)




