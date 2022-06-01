# Correlations: Changes

# TODO: Add CNN variables

# Load data --------------------------------------------------------------------
df <- readRDS(file.path(data_dir, SURVEY_NAME, "FinalData", "Merged Datasets", "survey_alldata_clean_changes.Rds"))

# Calc Correlations ------------------------------------------------------------
df_cor <- df %>%
  dplyr::filter(year_diff_max %in% T) %>%
  pivot_longer(cols = -c(country_code, gadm_uid, iso2, year_diff, year, lag, pca_allvars)) %>%
  dplyr::filter(!is.na(value)) %>%
  group_by(name, country_code, year, year_diff, lag) %>%
  dplyr::summarise(cor = cor(pca_allvars, value),
                   n_obs = n()) %>%
  dplyr::rename(variable = name) %>%
  clean_varnames() %>%
  dplyr::filter(!is.na(cor)) %>%
  dplyr::filter(n_obs >= 30) 

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

