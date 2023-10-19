# Correlations of Features with Poverty

# Products tables/figures of (1) all features and (2) select features

# Within country; show distribution. (color by continent?)

# Load data --------------------------------------------------------------------
df <- readRDS(file.path(data_dir, "DHS", "FinalData", "Merged Datasets", "survey_alldata_clean.Rds"))

fb_param_df <- readRDS(file.path(data_dir, "Facebook Marketing", "FinalData", "facebook_marketing_parameters_clean.Rds"))
gc_param_df <- read_csv(file.path(data_dir, "Globcover", "RawData", "gc_classes.csv"))

# Compute correlation ----------------------------------------------------------
# Compute correlation for each variable & country

### Data to long to make easier to compute correlation across countries + variables
df_long <- df %>%
  dplyr::select(country_name, 
                country_code,
                continent_adj,
                pca_allvars_mr,
                starts_with("fb_prop")) %>%
  pivot_longer(cols = -c(country_name, country_code, pca_allvars_mr, continent_adj)) %>%
  dplyr::rename(variable = name) 

### Compute Correlation
cor_df <- df_long %>%
  dplyr::filter(!is.na(pca_allvars_mr),
                !is.na(value)) %>%
  group_by(variable, country_name, country_code, continent_adj) %>%
  dplyr::summarise(cor = cor(pca_allvars_mr, value)) %>%
  ungroup() %>%
  
  # TODO: CHECK WHY THIS WOULD HAPPEN
  dplyr::filter(!is.na(cor))

### Clean select name for variables
## Facebook
fb_param_clean_df <- fb_param_df %>%
  dplyr::select(param_id, param_category, param_name_simple) %>%
  dplyr::mutate(variable = paste0("fb_prop_estimate_mau_upper_bound_", param_id),
                variable_clean = paste0(param_category, ": ", param_name_simple)) %>%
  dplyr::select(variable, variable_clean)

cor_df <- left_join(cor_df,
                    fb_param_clean_df,
                    by = "variable")

# Figure -----------------------------------------------------------------------
p <- cor_df %>%
  ggplot(aes(x = country_name,
             y = variable_clean,
             fill = cor,
             label = round(cor, 2))) +
  geom_tile(color = "white") +
  geom_text(color = "black",
            size = 2.5) +
  scale_fill_distiller(palette = "Spectral",
                       direction = 0) +
  labs(y = NULL,
       x = NULL,
       fill = expression(r^2)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.2, color = "black"),
        axis.text.y = element_text(color = "black"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) 

ggsave(p, filename = file.path(figures_global_dir, "fb_features_cor_each_country.png"),
       height = 9,
       width = 20)




