# Figure: Correlation of Changes in PCA Wealth Index vs Wealth Score

# Load data --------------------------------------------------------------------
df <- readRDS(file.path(data_dir, SURVEY_NAME, "FinalData", "Merged Datasets", "survey_alldata_clean_changes.Rds"))

df <- df %>%
  dplyr::filter(year_diff_max %in% T)

# Make title -------------------------------------------------------------------
# Title: "[iso2]; Cor = [cor]"
df <- df %>%
  #dplyr::mutate(wealth_index_score = wealth_index_score / 1000000) %>%
  group_by(iso2) %>%
  #dplyr::mutate(wealth_index_score = rescale(wealth_index_score, to = c(1,5))) %>%
  dplyr::mutate(cor = cor(wealth_index_score,
                          pca_allvars)) %>%
  ungroup() %>% 
  dplyr::mutate(title = paste0(iso2, "; Cor = ", round(cor, 2)))

# Figure: Scatterplots ---------------------------------------------------------
p <- df %>%
  ggplot() +
  geom_point(aes(x = wealth_index_score,
                 y = pca_allvars),
             size = 0.1) +
  labs(x = "Changes in DHS Wealth Index",
       y = "Change in Global Asset Index") +
  theme_classic() +
  theme(strip.background = element_blank(),
        strip.text = element_text(face = "bold")) +
  facet_wrap(~title)

ggsave(p, 
       filename = file.path(figures_global_dir, "dhsindex_globalindex_cor_changes.png"),
       height = 10,
       width = 8)

#### Stats
df_cor <- df %>%
  distinct(iso2, cor) 

table(df_cor$cor > 0.9)
mean(df_cor$cor > 0.9)

table(df_cor$cor > 0.8)
mean(df_cor$cor > 0.8)

df_cor$cor %>% summary()

