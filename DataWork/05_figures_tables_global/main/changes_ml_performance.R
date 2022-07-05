# ML Performance: Predicting Changes

# TODO: Include N // by N
# TODO: By year_diff (merge in)\

library(gghalves)


# For boxplots
FILL_COLOR <- "gray80" 

# For adding text onto boxplot
p75 <- function(x) quantile(x, probs = 0.75) %>% as.numeric()

# Load data --------------------------------------------------------------------
df <- readRDS(file.path(data_dir, SURVEY_NAME, "FinalData", "pov_estimation_results",
                        "accuracy_appended_bestparam.Rds"))

survey_df <- readRDS(file.path(data_dir, SURVEY_NAME, "FinalData", "Merged Datasets",
                               "survey_alldata_clean_changes_cluster_predictions.Rds"))

# Prep data --------------------------------------------------------------------
# ## Grab best parameters
# xg_param_set_best <- df %>%
#   dplyr::filter(level_change %in% "changes",
#                 feature_type %in% "all_changes",
#                 target_var %in% "pca_allvars",
#                 estimation_type %in% "within_country_cv") %>%
#   group_by(xg_param_set) %>%
#   dplyr::summarise(r2_mean = mean(r2),
#                    r2_median = median(r2),
#                    r2_max = max(r2)) %>%
#   arrange(-r2_median) %>%
#   head(1) %>%
#   pull(xg_param_set)
# 
# ## Filter dataframe
# df <- df %>%
#   dplyr::filter(level_change %in% "changes",
#                 xg_param_set %in% xg_param_set_best,
#                 feature_type %in% "all_changes",
#                 target_var %in% "pca_allvars",
#                 estimation_type %in% "within_country_cv")

df <- df %>%
  dplyr::filter(level_change %in% "changes")

# Boxplots: By Training Sample -------------------------------------------------
p_boxplot_tsample <- df %>%
  dplyr::filter(feature_type %in% "all_changes",
                target_var %in% "pca_allvars",
                estimation_type != "best") %>%
  ggplot(aes(x = reorder(estimation_type_clean, r2, FUN = mean, .desc =TRUE),
             y = r2)) +
  geom_half_boxplot(errorbar.draw = FALSE, center = TRUE,
                    fill = FILL_COLOR) +
  #geom_half_point(transformation = position_jitter(width = 0.05, height = 0.1)) +
  stat_summary(fun = median, geom = "text", col = "black",     # Add text to plot
               vjust = -0.2, hjust = 0.5, aes(label = paste(round(..y.., digits = 2)))) +
  stat_summary(fun = p75, geom = "text", col = "black",     # Add text to plot
               vjust = -0.2, hjust = -0.05, aes(label = paste(round(..y.., digits = 2)))) +
  stat_summary(fun = max, geom = "text", col = "firebrick3",    
               vjust = -0.2, aes(label = paste(round(..y.., digits = 2)))) +
  labs(x = NULL,
       y = expression(r^2),
       title = "C. Performance by training sample type") +
  #scale_y_continuous(limits = c(0,0.6)) +
  theme_classic() +
  theme(legend.position = "none",
        axis.text.y = element_text(face = "bold"),
        plot.title = element_text(face = "bold"),
        plot.title.position = "plot") +  
  coord_flip() 

# Boxplots: By Feature Set -----------------------------------------------------
p_boxplot_feature <- df %>%
  dplyr::filter(estimation_type %in% "within_country_cv",
                target_var %in% "pca_allvars") %>%
  ggplot(aes(x = reorder(feature_type_clean, r2, FUN = mean, .desc =TRUE),
             y = r2)) +
  geom_half_boxplot(errorbar.draw = FALSE, center = TRUE,
                    fill = FILL_COLOR) +
  #geom_half_point(transformation = position_jitter(width = 0.05, height = 0.1)) +
  stat_summary(fun = median, geom = "text", col = "black",     # Add text to plot
               vjust = -0.2, hjust = 0.5, aes(label = paste(round(..y.., digits = 2)))) +
  stat_summary(fun = p75, geom = "text", col = "black",     # Add text to plot
               vjust = -0.2, hjust = -0.01, aes(label = paste(round(..y.., digits = 2)))) +
  stat_summary(fun = max, geom = "text", col = "firebrick3",    
               vjust = -0.2, aes(label = paste(round(..y.., digits = 2)))) +
  labs(x = NULL,
       y = expression(r^2),
       title = "D. Performance using different features") +
  #scale_y_continuous(limits = c(0,0.6)) +
  theme_classic() +
  theme(legend.position = "none",
        axis.text.y = element_text(face = "bold"),
        plot.title = element_text(face = "bold"),
        plot.title.position = "plot") +  
  coord_flip() 

# Boxplots: By Target Variable -------------------------------------------------
p_boxplot_tvar <- df %>%
  dplyr::filter(feature_type %in% "all_changes",
                estimation_type %in% "within_country_cv") %>%
  ggplot(aes(x = reorder(target_var_clean, r2, FUN = mean, .desc =TRUE),
             y = r2)) +
  geom_half_boxplot(errorbar.draw = FALSE, center = TRUE,
                    fill = FILL_COLOR) +
  geom_half_point(transformation = position_jitter(width = 0.05, height = 0.005)) +
  stat_summary(fun = median, geom = "text", col = "black",     # Add text to plot
               vjust = -0.2, hjust = 0.5, aes(label = paste(round(..y.., digits = 2)))) +
  stat_summary(fun = p75, geom = "text", col = "black",     # Add text to plot
               vjust = -0.2, hjust = -0.05, aes(label = paste(round(..y.., digits = 2)))) +
  stat_summary(fun = max, geom = "text", col = "firebrick3",    
               vjust = -0.2, aes(label = paste(round(..y.., digits = 2)))) +
  labs(x = NULL,
       y = expression(r^2),
       title = "C. Performance by training sample type") +
  theme_classic() +
  theme(legend.position = "none",
        axis.text.y = element_text(face = "bold"),
        plot.title = element_text(face = "bold"),
        plot.title.position = "plot") +  
  coord_flip() 

# Boxplots: By District --------------------------------------------------------
dist_df <- bind_rows(
  survey_df %>%
    dplyr::select(country_code,
                  gadm_uid,
                  pca_allvars,
                  predict_pca_allvars_within_country_cv_all_changes) %>%
    dplyr::rename(truth = pca_allvars,
                  prediction = predict_pca_allvars_within_country_cv_all_changes) %>%
    dplyr::mutate(target_var_clean = "Asset Index"),
  
  survey_df %>%
    dplyr::select(country_code,
                  gadm_uid,
                  wealth_index_score,
                  predict_wealth_index_score_within_country_cv_all_changes) %>%
    dplyr::rename(truth = wealth_index_score,
                  prediction = predict_wealth_index_score_within_country_cv_all_changes) %>%
    dplyr::mutate(target_var_clean = "DHS Wealth Index") 
)

p_boxplot_tvar_district <- dist_df %>%
  ungroup() %>%
  group_by(country_code, gadm_uid, target_var_clean) %>%
  dplyr::summarise(truth = mean(truth),
                   prediction = mean(prediction)) %>%
  ungroup() %>%
  group_by(country_code, target_var_clean) %>%
  dplyr::summarise(r2 = cor(truth, prediction)^2) %>%
  
  # Make figure
  ggplot(aes(x = reorder(target_var_clean, r2, FUN = mean, .desc =TRUE),
             y = r2)) +
  geom_half_boxplot(errorbar.draw = FALSE, center = TRUE,
                    fill = FILL_COLOR) +
  geom_half_point(transformation = position_jitter(width = 0.05, height = 0.005)) +
  stat_summary(fun = median, geom = "text", col = "black",     # Add text to plot
               vjust = -0.2, hjust = 0.5, aes(label = paste(round(..y.., digits = 2)))) +
  stat_summary(fun = p75, geom = "text", col = "black",     # Add text to plot
               vjust = -0.2, hjust = -0.05, aes(label = paste(round(..y.., digits = 2)))) +
  stat_summary(fun = max, geom = "text", col = "firebrick3",    
               vjust = -0.2, aes(label = paste(round(..y.., digits = 2)))) +
  labs(x = NULL,
       y = expression(r^2),
       title = "C. Performance by training sample type") +
  theme_classic() +
  theme(legend.position = "none",
        axis.text.y = element_text(face = "bold"),
        plot.title = element_text(face = "bold"),
        plot.title.position = "plot") +  
  coord_flip() 

# Explain variation ------------------------------------------------------------
line_color <- "darkorange"
df_sum <- df %>%
  ungroup() %>%
  dplyr::filter(feature_type %in% "all_changes",
                estimation_type %in% "within_country_cv",
                target_var %in% "pca_allvars")

df_sum %>%
  dplyr::mutate(wdi_population = log(wdi_population)) %>%
  dplyr::select(r2, 
                year_diff,
                pca_allvars_sd_change,
                ntlharmon_avg_sd_change,
                wdi_population) %>%
  pivot_longer(cols = -r2) %>%
  dplyr::mutate(name = case_when(
    name == "pca_allvars_sd_change" ~ "A. Std. Dev. of Asset Index Change",
    name == "ntlharmon_avg_sd_change" ~ "B. Std. Dev. of NTL Change",
    name == "year_diff" ~ "C. Year Difference",
    name == "wdi_population" ~ "D. Population, logged",
    TRUE ~ name,
  )) %>%
  ggplot(aes(x = value,
             y = r2)) +
  geom_smooth(method='lm',
              se = F,
              color = line_color) +
  geom_point() +
  stat_poly_eq() +
  labs(x = NULL,
       y = "r2:\nTrue vs.\nPredicted\nAsset Index" ) +
  theme_classic() +
  theme(axis.title.y = element_text(angle = 0,
                                    vjust = 0.5),
        strip.background = element_blank(),
        strip.text = element_text(face = "bold",
                                  color = "black",
                                  hjust = 0,
                                  size = 12)) +
  facet_wrap(~name,
             scales = "free_x")


df_sum %>%
  ggplot(aes(x = year_diff,
             y = r2)) +
  geom_smooth(method='lm',
              se = F,
              color = line_color) +
  geom_point() +
  stat_poly_eq() +
  labs(x = "Year Difference",
       y = "r2:\nTrue vs.\nPredicted\nAsset Index" ) +
  theme_classic() +
  theme(axis.title.y = element_text(angle = 0,
                                    vjust = 0.5))

df_sum %>%
  ggplot(aes(x = pca_allvars_sd_change,
             y = r2)) +
  geom_smooth(method='lm',
              se = F,
              color = line_color) +
  geom_point() +
  stat_poly_eq() +
  labs(x = "Std. Dev. of Change in Asset Index",
       y = "r2:\nTrue vs.\nPredicted\nAsset Index" ) +
  theme_classic() +
  theme(axis.title.y = element_text(angle = 0,
                                    vjust = 0.5))

df_sum %>%
  ggplot(aes(x = log(wdi_population),
             y = r2)) +
  geom_smooth(method='lm',
              se = F,
              color = line_color) +
  geom_point() +
  stat_poly_eq() +
  labs(x = "Population, logged",
       y = "r2:\nTrue vs.\nPredicted\nAsset Index" ) +
  theme_classic() +
  theme(axis.title.y = element_text(angle = 0,
                                    vjust = 0.5))

df_sum %>%
  ggplot(aes(x = n,
             y = r2)) +
  geom_smooth(method='lm',
              se = F,
              color = line_color) +
  geom_point() +
  stat_poly_eq() +
  labs(x = "N DHS Clusters in Country",
       y = "r2:\nTrue vs.\nPredicted\nAsset Index" ) +
  theme_classic() +
  theme(axis.title.y = element_text(angle = 0,
                                    vjust = 0.5))


df_sum %>%
  ggplot(aes(x = log(ntlharmon_avg_sd_change+1),
             y = r2)) +
  geom_smooth(method='lm',
              se = F,
              color = "firebrick3") +
  geom_point() +
  stat_poly_eq() +
  labs(x = "N DHS Clusters in Country",
       y = "r2:\nTrue vs.\nPredicted\nAsset Index" ) +
  theme_classic() +
  theme(axis.title.y = element_text(angle = 0,
                                    vjust = 0.5))








# By Country -------------------------------------------------------------------
country_r2_df <- survey_df %>%
  dplyr::select(country_code, pca_allvars, wealth_index_score, 
                
                predict_pca_allvars_within_country_cv_all_changes,
                predict_pca_allvars_global_country_pred_all_changes,
                predict_pca_allvars_same_continent_all_changes,
                predict_pca_allvars_other_continents_all_changes,
                
                predict_wealth_index_score_within_country_cv_all_changes) %>%
  group_by(country_code) %>%
  dplyr::summarise(r2 = cor(predict_wealth_index_score_within_country_cv_all_changes, wealth_index_score)^2,
                   n = n()) %>%
  ungroup()

country_r2_df$r2 %>% hist()

survey_df$predict_pca_allvars_other_continents_all_changes



# Figure: Scatterplot ----------------------------------------------------------
r2_all_d <- cor(survey_df$pca_allvars,   
                survey_df$predict_pca_allvars_within_country_cv_all_changes)^2

p_scatter <- survey_df %>%
  ggplot(aes(x = pca_allvars,
             y = predict_pca_allvars_within_country_cv_all_changes)) +
  geom_point(size = 0.5) +
  geom_richtext(aes(label = paste0("r<sup>2</sup>: ", round(r2_all_d,2)),
                    x = -3.5,
                    y = 2),
                color = "black",
                hjust = 0,
                fill = NA, 
                label.color = NA,
                size = 4.5) +
  labs(x = "True Change in Wealth",
       y = "Predicted Changed in Wealth",
       title = "A. Estimated vs. True Change in Wealth Asset Index") +
  theme_minimal() +
  theme(legend.position = c(0.9, 0.1),
        legend.box.background = element_rect(colour = "black"),
        plot.title = element_text(face = "bold"),
        axis.title = element_text(face = "bold"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) 

# Cluster Scatterplot: By Continent --------------------------------------------
survey_df <- survey_df %>%
  group_by(continent_adj) %>%
  dplyr::mutate(r2 = cor(pca_allvars, predict_pca_allvars_within_country_cv_all_changes)^2) %>%
  ungroup() %>%
  dplyr::mutate(panel_name = paste0(continent_adj, "\nr2 = ", round(r2,2)))

survey_df %>%
  ggplot(aes(x = pca_allvars,
             y = predict_pca_allvars_within_country_cv_all_changes)) +
  geom_point(size = 0.5) +
  #geom_rug(col=rgb(.5,0,0,alpha=.2)) +
  facet_wrap(~panel_name) +
  labs(x = "True Asset Index",
       y = "Predicted Asset Index") +
  theme_classic() +
  theme(legend.position = c(0.9, 0.1),
        legend.box.background = element_rect(colour = "black"),
        plot.title = element_text(face = "bold"),
        axis.title = element_text(face = "bold"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        strip.background = element_blank()) 

# ADM2 Scatterplot: By Continent -----------------------------------------------
survey_adm2_df <- survey_df %>%
  group_by(gadm_uid, country_code, continent_adj) %>%
  summarise_if(is.numeric, mean)

survey_adm2_df <- survey_adm2_df %>%
  group_by(continent_adj) %>%
  dplyr::mutate(r2 = cor(pca_allvars, predict_pca_allvars_within_country_cv_all_changes)^2) %>%
  ungroup() %>%
  dplyr::mutate(panel_name = paste0(continent_adj, "\nr2 = ", round(r2,2)))

survey_adm2_df %>%
  ggplot(aes(x = pca_allvars,
             y = predict_pca_allvars_within_country_cv_all_changes)) +
  geom_point(size = 0.5) +
  #geom_rug(col=rgb(.5,0,0,alpha=.2)) +
  facet_wrap(~panel_name) +
  labs(x = "True Asset Index",
       y = "Predicted Asset Index") +
  theme_classic() +
  theme(legend.position = c(0.9, 0.1),
        legend.box.background = element_rect(colour = "black"),
        plot.title = element_text(face = "bold"),
        axis.title = element_text(face = "bold"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        strip.background = element_blank()) 

# By type --------------------------------------------------------------
# Continent and
# --Feature type
# --Estimation type (within_cv, global)

survey_sum_df <- survey_df %>%
  dplyr::select(continent_adj,
                pca_allvars,
                contains("predict_pca_allvars_within_country_cv")) %>%
  pivot_longer(cols = -c(pca_allvars, continent_adj)) %>%
  group_by(name, continent_adj) %>%
  dplyr::summarise(r2 = cor(pca_allvars, value)^2) 

survey_sum_df %>%
  ggplot() +
  geom_col(aes(y = name,
               x = r2)) +
  facet_wrap(~continent_adj)

# By country characteristic? -----------------------------------------------
# Instead of splitting by continents, split by N years diff -- or split generally
# by different characteristics. So don't do at country level. Just split EVERYTHING!!!!!

survey_df %>%
  dplyr::mutate(year_diff_bin = case_when(
    year_diff %in% 4:5 ~ 5,
    year_diff %in% 6:10 ~ 10,
    year_diff %in% 11:15 ~ 15,
    year_diff %in% 16:20 ~ 20
  )) %>%
  group_by(year_diff_bin) %>%
  dplyr::summarise(r2 = cor(pca_allvars, predict_pca_allvars_within_country_cv_all_changes)^2) %>%
  ungroup() 

survey_df$year_diff %>%
  table()

# OLD ==========================================================================
# OLD ==========================================================================
# OLD ==========================================================================
# OLD ==========================================================================
# OLD ==========================================================================
# OLD ==========================================================================

# Figure: Map ------------------------------------------------------------------
## Stats
cor_mean_all <- mean(df_best$r2)
cor_mean_africa <- mean(df_best$r2[df_best$continent_adj %in% "Africa"])
cor_mean_americas <- mean(df_best$r2[df_best$continent_adj %in% "Americas"])
cor_mean_eurasia <- mean(df_best$r2[df_best$continent_adj %in% "Eurasia"])

## Prep data
world_sp <- ne_countries(type = "countries", scale=50)

world_sp <- world_sp[world_sp$continent != "Antarctica",]

world_sp@data <- world_sp@data %>%
  dplyr::select(name, continent) %>%
  dplyr::rename(country_name = name) %>%
  dplyr::mutate(country_name = case_when(
    country_name %in% "Dem. Rep. Congo" ~ "Congo - Kinshasa",
    country_name %in% "Côte d'Ivoire" ~ "Côte d’Ivoire",
    country_name %in% "Dominican Rep." ~ "Dominican Republic",
    country_name %in% "Swaziland" ~ "Eswatini",
    country_name %in% "Myanmar" ~ "Myanmar (Burma)",
    TRUE ~ country_name
  ))

world_sp <- merge(world_sp, df_best, by = "country_name", all.x = T, all.y = F)

world_sp$id <- row.names(world_sp)
world_sp_tidy <- tidy(world_sp)
world_sp_tidy <- merge(world_sp_tidy, world_sp@data)

## Figure
TEXT_X_MAP <- -73
TEXT_Y_TOP_MAP <- 40
TEXT_Y_INC_MAP <- 4
FONT_SIZE <- 4.5

p_map <- ggplot() +
  geom_polygon(data = world_sp_tidy[is.na(world_sp_tidy$cor),],
               aes(x = long, y = lat, group = group),
               fill = "gray50",
               color = "white") +
  geom_polygon(data = world_sp_tidy[!is.na(world_sp_tidy$cor),],
               aes(x = long, y = lat, group = group,
                   fill = r2),
               color = "black") +
  geom_richtext(aes(label = paste0("All - Avg r<sup>2</sup>: ", round(cor_mean_all,2)),
                    x = TEXT_X_MAP,
                    y = TEXT_Y_TOP_MAP),
                color = "black",
                hjust = 0,
                fill = NA, 
                label.color = NA,
                size = FONT_SIZE) +
  geom_richtext(aes(label = paste0("Africa - Avg r<sup>2</sup>: ", round(cor_mean_africa,2)),
                    x = TEXT_X_MAP,
                    y = TEXT_Y_TOP_MAP - TEXT_Y_INC_MAP),
                color = "black",
                hjust = 0,
                fill = NA, 
                label.color = NA,
                size = FONT_SIZE) +
  geom_richtext(aes(label = paste0("Americas - Avg r<sup>2</sup>: ", round(cor_mean_americas,2)),
                    x = TEXT_X_MAP,
                    y = TEXT_Y_TOP_MAP - TEXT_Y_INC_MAP*2),
                color = "black",
                hjust = 0,
                fill = NA, 
                label.color = NA,
                size = FONT_SIZE) +
  geom_richtext(aes(label = paste0("Eurasia - Avg r<sup>2</sup>: ", round(cor_mean_eurasia,2)),
                    x = TEXT_X_MAP,
                    y = TEXT_Y_TOP_MAP - TEXT_Y_INC_MAP*3),
                color = "black",
                hjust = 0,
                fill = NA, 
                label.color = NA,
                size = FONT_SIZE) +
  scale_fill_distiller(palette = "Spectral",
                       direction = 0) +
  #scale_fill_distiller(palette = "Spectral",
  #                     direction = 0,
  #                     limits = c(0.3, 0.949),
  #                     #values = c(0.3, 0.4, 0.5, 0.6, 0.7, 0.8),
  #                     labels = c("<0.3", "0.4", "0.5", "0.6", "0.7", "0.8", "0.9")) +
  labs(fill = expression(r^2),
       title = "B. r2 of Estimated vs True Change in Wealth Asset Index Within Countries", 
       caption = "") +
  theme_void() +
  coord_quickmap() +
  coord_cartesian(xlim=c(-85,135),
                  ylim=c(-34, 45)) +
  theme(plot.title = element_text(face = "bold"),
        legend.position = c(0.05,0.18)) 

# Boxplots: By Training Sample -------------------------------------------------

p_boxplot_tsample <- df %>%
  dplyr::filter(feature_type %in% "all_changes",
                target_var %in% "pca_allvars",
                estimation_type != "best") %>%
  ggplot(aes(x = reorder(estimation_type_clean, r2, FUN = mean, .desc =TRUE),
             y = r2)) +
  geom_half_boxplot(errorbar.draw = FALSE, center = TRUE,
                    fill = FILL_COLOR) +
  #geom_half_point(transformation = position_jitter(width = 0.05, height = 0.1)) +
  stat_summary(fun = median, geom = "text", col = "black",     # Add text to plot
               vjust = -0.2, hjust = 0.5, aes(label = paste(round(..y.., digits = 2)))) +
  stat_summary(fun = p75, geom = "text", col = "black",     # Add text to plot
               vjust = -0.2, hjust = -0.05, aes(label = paste(round(..y.., digits = 2)))) +
  stat_summary(fun = max, geom = "text", col = "firebrick3",    
               vjust = -0.2, aes(label = paste(round(..y.., digits = 2)))) +
  labs(x = NULL,
       y = expression(r^2),
       title = "C. Performance by training sample type") +
  #scale_y_continuous(limits = c(0,0.6)) +
  theme_classic() +
  theme(legend.position = "none",
        axis.text.y = element_text(face = "bold"),
        plot.title = element_text(face = "bold"),
        plot.title.position = "plot") +  
  coord_flip() 

# Boxplots: By Feature Set -----------------------------------------------------
p_boxplot_feature <- df %>%
  dplyr::filter(estimation_type %in% "best",
                target_var %in% "pca_allvars") %>%
  ggplot(aes(x = reorder(feature_type_clean, r2, FUN = mean, .desc =TRUE),
             y = r2)) +
  geom_half_boxplot(errorbar.draw = FALSE, center = TRUE,
                    fill = FILL_COLOR) +
  #geom_half_point(transformation = position_jitter(width = 0.05, height = 0.1)) +
  stat_summary(fun = median, geom = "text", col = "black",     # Add text to plot
               vjust = -0.2, hjust = 0.5, aes(label = paste(round(..y.., digits = 2)))) +
  stat_summary(fun = p75, geom = "text", col = "black",     # Add text to plot
               vjust = -0.2, hjust = -0.01, aes(label = paste(round(..y.., digits = 2)))) +
  stat_summary(fun = max, geom = "text", col = "firebrick3",    
               vjust = -0.2, aes(label = paste(round(..y.., digits = 2)))) +
  labs(x = NULL,
       y = expression(r^2),
       title = "D. Performance using different features") +
  #scale_y_continuous(limits = c(0,0.6)) +
  theme_classic() +
  theme(legend.position = "none",
        axis.text.y = element_text(face = "bold"),
        plot.title = element_text(face = "bold"),
        plot.title.position = "plot") +  
  coord_flip() 

# Arrange and export -----------------------------------------------------------
p_row1 <- ggarrange(p_scatter,
                    p_map,
                    nrow = 1,
                    widths = c(0.4, 0.6))

p_row2 <- ggarrange(p_boxplot_tsample,
                    p_boxplot_feature,
                    nrow = 1)

p_all <- ggarrange(p_row1,
                   p_row2,
                   ncol = 1)

ggsave(p_all, 
       filename = file.path(figures_global_dir, "ml_changes_results.png"),
       height = 9,
       width = 15)


