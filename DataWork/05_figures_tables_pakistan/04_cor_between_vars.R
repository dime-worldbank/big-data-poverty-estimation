# Correlation between variables
# Pakistan analysis

# https://stackoverflow.com/questions/57861765/reorder-axis-labels-of-correlation-matrix-plot

# Load data --------------------------------------------------------------------
survey_df <- readRDS(file.path(data_dir, SURVEY_NAME, "FinalData", "Merged Datasets", "survey_alldata_clean.Rds"))

survey_df <- survey_df %>%
  dplyr::filter(country_code %in% "PK") 

# Select variables -------------------------------------------------------------
# Use variable with strongest correlation in each dataset with wealth_score

cor_df <- survey_df %>%
  dplyr::select(uid,
                country_name, 
                continent_adj,
                gadm_uid,
                pca_allvars,
                wealth_index_score,
                starts_with("viirs_"),
                starts_with("gc_"),
                starts_with("osm_"),
                starts_with("l8_"),
                starts_with("cnn_"),
                starts_with("fb_prop"),
                starts_with("worldclim_"),
                starts_with("worldpop_"),
                starts_with("elevslope_"),
                starts_with("globalmod_"),
                starts_with("pollution_aod_"),
                starts_with("pollution_s5p_"),
                starts_with("weather_")) %>%
  pivot_longer(cols = -c(uid, gadm_uid, country_name, continent_adj,
                         pca_allvars, wealth_index_score)) %>%
  dplyr::rename(variable = name) %>%
  clean_varnames() %>%
  group_by(variable, variable_cat) %>%
  dplyr::summarise(cor = cor(wealth_index_score, value)) %>%
  ungroup() %>%
  dplyr::mutate(cor_abs = abs(cor)) %>%
  dplyr::filter(!is.na(cor))

cor_sub_df <- cor_df %>%
  group_by(variable_cat) %>%
  slice_max(order_by = cor, n = 1) %>%
  arrange(-cor)

vars_to_use <- cor_sub_df %>%
  ungroup() %>%
  pull(variable)

# Prep data --------------------------------------------------------------------
cor_var_df <- survey_df %>%
  dplyr::select(all_of(vars_to_use)) %>%
  cor() %>%
  as.data.frame() %>%
  rownames_to_column(var = "variable") %>%
  clean_varnames()

row.names(cor_var_df) <- cor_var_df$variable_clean_with_dataset
cor_var_df <- cor_var_df %>%
  dplyr::select(-c(variable, variable_cat, variable_clean, variable_clean_with_dataset, variable_clean_short))
names(cor_var_df) <- row.names(cor_var_df)
cor_var_mat <- as.matrix(cor_var_df)

#cor_var_mat_melt <- reshape2::melt(cor_var_mat)

# Get upper triangle of the correlation matrix
get_upper_tri <- function(cor_var_mat){
  cor_var_mat[lower.tri(cor_var_mat)]<- NA
  return(cor_var_mat)
}

cor_var_mat_melt <- cor_var_mat %>%
  get_upper_tri() %>%
  reshape2::melt() %>%
  dplyr::mutate(value = case_when(
    Var1 == Var2 ~ NA_real_,
    TRUE ~ value
  ))

# Merge in correlation with wealth_score
cor_sub_clean_df <- cor_sub_df %>%
  clean_varnames() %>%
  dplyr::select(variable_clean_with_dataset, cor) %>%
  dplyr::rename(cor_wealth_score = cor) 

cor_var_mat_melt <- cor_var_mat_melt %>%
  merge(cor_sub_clean_df, by.x = "Var1", by.y = "variable_clean_with_dataset") %>%
  merge(cor_sub_clean_df, by.x = "Var2", by.y = "variable_clean_with_dataset")

# Figure -----------------------------------------------------------------------
p_cor_wealth <- cor_sub_df %>%
  clean_varnames() %>%
  dplyr::mutate(label_move = case_when(
    cor >= 0 ~ 0.17,
    cor < 0 ~ -0.17
  )) %>%
  ggplot(aes(xmin = 0,
             xmax = cor,
             x = cor,
             y = reorder(variable_clean_with_dataset, cor))) +
  geom_linerange() +
  geom_point() +
  geom_text(aes(label = round(cor, 2),
                x = cor + label_move)) +
  labs(x = paste0("",
                  paste(rep("\n", 13), collapse = "")),
       y = NULL,
       title = "B. Correlation with\nWealth Score") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank()) +
  scale_x_continuous(limits = c(-0.5, 0.9))

p_cor_vars <- cor_var_mat_melt %>%
  ggplot(aes(x = reorder(Var1,  cor_wealth_score.x),
             y = reorder(Var2,  cor_wealth_score.y),
             fill = value)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(value, 2))) +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       na.value = "white",
                       name="Correlation") +
  scale_y_discrete(position = "right") +
  theme_minimal()+ 
  theme(axis.text.y.right = element_text(hjust = 0.5,
                                         color = "black",
                                         size = 11),
        axis.text.x.bottom = element_text(angle = 45, 
                                          hjust = 1,
                                          color = "black",
                                          size = 11),
        
        legend.position = c(0.2, 0.8),
        plot.title = element_text(face = "bold")) +
  coord_fixed() +
  labs(x = NULL,
       y = NULL,
       title = "A. Correlation Between Variables\n")

p <- ggarrange(p_cor_vars + theme(plot.margin=unit(c(0,-3,0,0), "cm")), 
          p_cor_wealth + theme(plot.margin=unit(c(0,0,0,-3), "cm")), 
          widths = c(0.92, 0.08))

ggsave(p, filename = file.path(figures_pak_dir, "cor_between_vars.png"),
       height = 8, width = 13)


