# LSMS Results

# Load data --------------------------------------------------------------------
pred_df <- file.path(data_dir, "DHS", "FinalData", "pov_estimation_results", 
                     "predictions") %>%
  list.files(pattern = "*.Rds",
             full.names = T) %>%
  str_subset("lsms") %>%
  map_df(readRDS) 

pred_df <- pred_df %>%
  dplyr::mutate(country_name = case_when(
    country_code == "BF" ~ "Burkina Faso",
    country_code == "BJ" ~ "Benin",
    country_code == "CI" ~ "Cote d'Ivoire",
    country_code == "ET" ~ "Ethiopia",
    country_code == "MW" ~ "Malawi",
    country_code == "TG" ~ "Togo"
  ))

pred_df <- pred_df %>%
  filter(ml_model_type %in% "xgboost",
         estimation_type %in% "within_country_cv") # global_country_pred, within_country_cv

# Main results -----------------------------------------------------------------
#### PCA
pred_sub_df <- pred_df %>%
  filter(feature_type %in% "all_lsms",
         target_var %in% "pca_allvars_mr")

v_min <- c(pred_sub_df$prediction, pred_sub_df$truth) %>% min()
v_max <- c(pred_sub_df$prediction, pred_sub_df$truth) %>% max() 

r2_R2_df <- pred_sub_df %>%
  group_by(country_name) %>%
  dplyr::summarise(r2 = cor(prediction, truth)^2,
                   R2 = R2(prediction, truth, form = "traditional")) %>%
  ungroup() %>%
  mutate(label = paste0("r<sup>2</sup>: ", round(r2, 2), "<br>R<sup>2</sup>: ", round(R2, 2)))

p_pca <- pred_sub_df %>%
  ggplot(aes(x = prediction,
             y = truth)) +
  geom_richtext(data = r2_R2_df,
                aes(x = 0, y = 3.5, label = label),
                fill = NA, 
                label.color = NA,
                color = "red") +
  geom_point(size = 0.1,
             alpha = 0.5) +
  geom_smooth(method='lm',
              se = F,
              size = 0.5,
              color = "darkorange") +
  # stat_cor(aes(label = paste(gsub("R", "r", ..rr.label..), sep = "*`,`~")),
  #          label.x.npc = "left",
  #          color = "firebrick3") +
  facet_wrap(~country_name, nrow = 1) +
  labs(x = "Predicted Asset Wealth",
       y = "True Asset Wealth",
       title = "A. Asset Wealth") +
  scale_x_continuous(limits = c(v_min, v_max)) +
  theme_classic2() +
  theme(strip.background = element_blank(),
        axis.title = element_text(size = 10),
        axis.text = element_text(size = 8),
        strip.text = element_text(face = "bold"),
        plot.title = element_text(face = "bold"))

#### Consumption
pred_sub_df <- pred_df %>%
  filter(feature_type %in% "all_lsms",
         target_var %in% "poverty_measure") 

v_min <- c(pred_sub_df$prediction, pred_sub_df$truth) %>% min() 
v_max <- c(pred_sub_df$prediction, pred_sub_df$truth) %>% max()
v_max <- v_max + 1000

r2_R2_df <- pred_sub_df %>%
  group_by(country_name) %>%
  dplyr::summarise(r2 = cor(prediction, truth)^2,
                   R2 = R2(prediction, truth, form = "traditional")) %>%
  ungroup() %>%
  mutate(label = paste0("r<sup>2</sup>: ", round(r2, 2), "<br>R<sup>2</sup>: ", round(R2, 2)))

p_cons <- pred_sub_df %>%
  ggplot(aes(x = prediction,
             y = truth)) +
  geom_richtext(data = r2_R2_df,
                aes(x = 3500, y = 10000, label = label),
                fill = NA, 
                label.color = NA,
                color = "red") +
  geom_point(size = 0.1,
             alpha = 0.5) +
  geom_smooth(method='lm',
              se = F,
              size = 0.5,
              color = "darkorange") +
  # stat_cor(aes(label = paste(gsub("R", "r", ..rr.label..), sep = "*`,`~")),
  #          label.x.npc = "left",
  #          color = "firebrick3") +
  facet_wrap(~country_name, nrow = 1) +
  labs(x = "Predicted Consumption",
       y = "True Consumption",
       title = "B. Consumption") +
  scale_x_continuous(limits = c(v_min, v_max)) +
  theme_classic2() +
  theme(strip.background = element_blank(),
        axis.title = element_text(size = 10),
        axis.text = element_text(size = 8),
        strip.text = element_text(face = "bold"),
        plot.title = element_text(face = "bold"))

p_all <- ggarrange(p_pca, 
                   p_cons,
                   nrow = 2)

ggsave(p_all,
       filename = file.path(figures_global_dir, "lsms_r2.png"),
       height = 5,
       width = 10)
