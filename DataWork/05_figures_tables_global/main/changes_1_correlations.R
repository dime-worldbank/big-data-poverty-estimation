# Correlations: Changes

# TODO: Rank cor?

# Load data --------------------------------------------------------------------
df <- readRDS(file.path(data_dir, SURVEY_NAME, "FinalData", "Merged Datasets", "survey_alldata_clean_changes_cluster.Rds"))

# Calc Correlations ------------------------------------------------------------
df_cor <- df %>%
  dplyr::select(-c(latitude, longitude)) %>%
  pivot_longer(cols = -c(uid, within_country_fold, continent_adj, country_code, country_name, gadm_uid, iso2, year_diff, 
                         pca_allvars, wealth_index_score, 
                         urban_rural_yr1, urban_rural_yr2)) %>%
  dplyr::filter(!is.na(value)) %>%
  group_by(name, country_code, year_diff) %>%
  dplyr::summarise(cor = cor(pca_allvars, value),
                   n_obs = n()) %>%
  dplyr::rename(variable = name) %>%
  clean_varnames() %>%
  dplyr::filter(!is.na(cor)) %>%
  
  # Count number of countries. For example, if variable has no variation in 
  # variable, then correlation will be NA. Only keep variables if all countries
  # have variation in data.
  group_by(variable) %>%
  dplyr::mutate(n_country = n()) %>%
  ungroup() %>%
  dplyr::filter(n_country == max(n_country))

## Take top 2 in each category
vars_to_use <- df_cor %>%
  group_by(variable,
           variable_cat) %>%
  dplyr::summarise(cor = abs(mean(cor))) %>%
  #arrange(cor) %>%
  ungroup() %>%
  group_by(variable_cat) %>%
  slice_max(cor, n = 2) %>%
  ungroup() %>%
  pull(variable)

df_cor <- df_cor[df_cor$variable %in% vars_to_use,]

# Figure -----------------------------------------------------------------------
df_cor %>%
  ggplot() + 
  geom_vline(xintercept = 0, color = "black") +
  geom_vline(xintercept = -0.25, alpha = 0.1) +
  geom_vline(xintercept = 0.25, alpha = 0.1) +
  geom_vline(xintercept = -0.5, alpha = 0.1) +
  geom_vline(xintercept = 0.5, alpha = 0.1) +
  geom_boxplot(aes(x = cor,
                   y = reorder(variable_clean, cor, FUN = median, .desc =TRUE),
                   fill = variable_cat)) +
  theme_classic() +
  theme(axis.text.y = element_text(color = "black"),
        plot.title = element_text(face = "bold", size = 10),
        plot.title.position = "plot") +
  labs(y = NULL,
       x = "Correlation",
       fill = "Variable\nCategory",
       title = "Correlation Between Changes in Variables and Wealth Index") +
  scale_x_continuous(breaks = seq(from = -.5, to = 0.5, by = 0.25))

ggsave(filename = file.path(figures_global_dir, "cor_changes.png"),
       height = 3,
       width = 9.5)

# Stats ------------------------------------------------------------------------
head(df_cor)

df_cor %>%
  filter(variable == "ntlharmon_avg") %>%
  pull(cor) %>%
  summary()

df_cor %>%
  filter(variable == "ntlharmon_avg") %>%
  arrange(-cor) %>%
  head()

df %>%
  filter(country_code == "MZ") %>%
  ggplot() +
  geom_point(aes(x = pca_allvars,
                 y = ntlharmon_avg))





