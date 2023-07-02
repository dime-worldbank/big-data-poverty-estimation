# LSMS Poverty Measure Correlation

# Load data --------------------------------------------------------------------
df <- readRDS(file.path(data_dir, "LSMS", "FinalData", "Merged Datasets", 
                        "survey_alldata_clean.Rds"))

p <- df %>%
  ggplot(aes(x = pca_allvars_mr,
             y = poverty_measure)) +
  geom_point(alpha = 0.5,
             size = 0.3) +
  
  geom_smooth(method = lm, se = F, color = "darkorange") +
  stat_cor(aes(label = paste(gsub("R", "r", ..rr.label..), ..p.label.., sep = "*`,`~")),
           label.x.npc = "left",
           color = "firebrick3") +
  
  facet_wrap(~country_name) +
  labs(x = "Wealth Asset Index",
       y = "Consumption") +
  theme_classic2() +
  theme(strip.background = element_blank(),
        strip.text = element_text(face = "bold"))

ggsave(p,
       filename = file.path(figures_global_dir, 
                            "lsms_pov_measure_cor.png"),
       height = 4,
       width = 6)

