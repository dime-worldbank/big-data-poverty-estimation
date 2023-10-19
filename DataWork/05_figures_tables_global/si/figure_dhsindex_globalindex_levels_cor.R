# DHS Index vs Global Asset Index

# Load data --------------------------------------------------------------------
df <- readRDS(file.path(data_dir, "DHS", "FinalData", "Merged Datasets", "survey_alldata_clean.Rds"))

df <- df[df$most_recent_survey %in% T,]

# Make title -------------------------------------------------------------------
# Title: "[iso2]; Cor = [cor]"
df <- df %>%
  group_by(iso2) %>%
  dplyr::mutate(wealth_index_score = rescale(wealth_index_score, to = c(1,5))) %>%
  dplyr::mutate(cor = cor(wealth_index_score,
                          pca_allvars_mr)) %>%
  ungroup() %>% 
  dplyr::mutate(title = country_name)

# Figure: Scatterplots ---------------------------------------------------------
p <- df %>%
  ggplot(aes(x = wealth_index_score,
             y = pca_allvars_mr)) +
  stat_poly_eq(small.r = T, 
               size = 3,
               color = "red") +
  geom_point(size = 0.1) +
  labs(x = "DHS Wealth Index",
       y = "Global\nWealth\nIndex") +
  theme_classic() +
  theme(strip.background = element_blank(),
        strip.text = element_text(face = "bold"),
        axis.title.y = element_text(angle = 0, vjust = 0.5)) +
  facet_wrap(~title)

ggsave(p, 
       filename = file.path(figures_global_dir, "dhsindex_globalindex_cor.png"),
       height = 10,
       width = 8)

#### Stats
df_cor <- df %>%
  distinct(iso2, cor) 

table(df_cor$cor > 0.9)
mean(df_cor$cor > 0.9)



