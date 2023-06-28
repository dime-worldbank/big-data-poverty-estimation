# Global Scatterplot - Predicted vs True

# TODO:
# (1) Global correlation using globally best parameters
# (2) Individual country correlation using globally best parameters
# (3) Individual country correlation using country-specific best parmeters
# ------For this, can load all predictions, compute correlations, and take
# ------best across countries. In this figure (or table), report estimation type

p_list_i <- 1
p_list <- list()
for(aggregate_district in c(F, T)){
  
  # # Load Data --------------------------------------------------------------------
  # ## Grab best parameters
  # acc_df <- readRDS(file.path(data_dir, SURVEY_NAME, "FinalData", "pov_estimation_results",
  #                             "accuracy_appended_bestparam_within_country_cv_levels.Rds"))
  # 
  # xg_param_set_use <- acc_df %>%
  #   head(1) %>%
  #   dplyr::mutate(xg_eta = xg_eta %>% str_replace_all("[:punct:]", ""),
  #                 xg_subsample = xg_subsample %>% str_replace_all("[:punct:]", "")) %>%
  #   dplyr::mutate(xg_param_set = paste(xg_max.depth,
  #                                      xg_eta,
  #                                      xg_nthread,
  #                                      xg_nrounds,
  #                                      xg_subsample,
  #                                      xg_objective,
  #                                      xg_min_child_weight,
  #                                      sep = "_")) %>%
  #   pull(xg_param_set) %>%
  #   str_replace_all(":", "") %>%
  #   str_replace_all("error_", "error")
  # 
  # ## Load predictions
  # pred_df <- file.path(data_dir, "DHS", "FinalData", "pov_estimation_results", "predictions") %>%
  #   #list.files(pattern = "predictions_within_country_cv_", # predictions_global_country_pred_ ""
  #   #           full.names = T) %>%
  #   list.files(pattern = "*.Rds",
  #              full.names = T) %>%
  #   str_subset("_levels_") %>%
  #   str_subset("pca_allvars_mr") %>%
  #   str_subset(xg_param_set_use) %>% # Parameter type
  #   map_df(readRDS)
  # 
  # pred_df <- pred_df %>%
  #   dplyr::filter(feature_type %in% "all")
  
  # Select best estimation type for each country ---------------------------------
  # pred_df <- pred_df %>%
  #   mutate(country_est_id = paste(estimation_type, country_code))
  # 
  # cor_df <- pred_df %>%
  #   group_by(country_est_id, estimation_type, country_code) %>%
  #   dplyr::summarise(cor = cor(truth, prediction)) %>%
  #   
  #   group_by(country_code) %>% 
  #   slice_max(order_by = cor, n = 1) %>%
  #   
  #   mutate(r2 = cor^2)
  # 
  # pred_df <- pred_df[pred_df$country_est_id %in% cor_df$country_est_id,]
  
  #pred_df <- pred_df[pred_df$estimation_type %in% "continent",]
  
  # Merge with select survey variables -------------------------------------------
  survey_df <- readRDS(file.path(data_dir, SURVEY_NAME, "FinalData", "Merged Datasets",
                                 "survey_alldata_clean_predictions.Rds"))
  
  survey_df <- survey_df %>%
    filter(most_recent_survey %in% T)
  
  # survey_df <- survey_df %>%
  #   dplyr::select(uid, latitude, longitude, continent_adj, urban_rural,
  #                 country_name, gadm_uid)
  # 
  # pred_df <- pred_df %>%
  #   dplyr::left_join(survey_df, by = "uid")
  # 
  # pred_df <- pred_df %>%
  #   dplyr::mutate(urban_rural = case_when(
  #     urban_rural == "U" ~ "Urban",
  #     urban_rural == "R" ~ "Rural"
  #   ))
  
  # Aggregate --------------------------------------------------------------------
  if(aggregate_district){
    survey_df <- survey_df %>%
      
      dplyr::filter(!is.na(predict_pca_allvars_mr_best)) %>%
      dplyr::filter(!is.na(predict_pca_allvars_mr_global_country_pred_all)) %>%
      
      dplyr::mutate(gadm_uid = paste(gadm_uid, country_code)) %>%
      group_by(gadm_uid, country_code, country_name, continent_adj) %>%
      dplyr::summarise(pca_allvars_mr = mean(pca_allvars_mr),
                       predict_pca_allvars_mr_best = mean(predict_pca_allvars_mr_best),
                       predict_pca_allvars_mr_global_country_pred_all = mean(predict_pca_allvars_mr_global_country_pred_all)) %>%
      ungroup()
  
  }
  
  # One dataset per estimation type --------------------------------------------
  # pred_wthn_cntry_cv_df = pred_df %>%
  #   dplyr::filter(estimation_type %in% "within_country_cv")
  
  # pred_best_df = pred_df %>%
  #   dplyr::filter(estimation_type %in% "best")
  # 
  # pred_global_df = pred_df %>%
  #   dplyr::filter(estimation_type %in% "global_country_pred") 
  
  pred_best_df <- survey_df %>%
    dplyr::rename(truth = pca_allvars_mr,
                  prediction = predict_pca_allvars_mr_best)
  
  pred_global_df <- survey_df %>%
    dplyr::rename(truth = pca_allvars_mr,
                  prediction = predict_pca_allvars_mr_global_country_pred_all)
  
  # Scatterplot ------------------------------------------------------------------
  if(aggregate_district){
    
    r2_all_d   <- cor(pred_global_df$truth,   pred_global_df$prediction)^2
    
    TEXT_X <- -4
    TEXT_Y_TOP <- 4
    TEXT_Y_INCR <- 0.45
    FONT_SIZE <- 4.5
    
    values <- c(pred_global_df$truth, pred_global_df$prediction)
    
    p_list[[p_list_i]] <- ggplot() +
      geom_point(data = pred_global_df,
                 aes(y = truth,
                     x = prediction),
                 color = "black",
                 size = 0.3,
                 alpha = 0.8) +
      geom_richtext(aes(label = paste0("All r<sup>2</sup>: ", round(r2_all_d,2)),
                        x = TEXT_X,
                        y = TEXT_Y_TOP),
                    color = "black",
                    hjust = 0,
                    fill = NA, 
                    label.color = NA,
                    size = FONT_SIZE) +
      scale_color_manual(values = c("chartreuse4", "chocolate2")) +
      labs(color = NULL,
           title = ifelse(aggregate_district,
                          "C. Estimated vs. true wealth index [district]",
                          "A. Estimated vs. true wealth index [cluster]"),
           y = "True Wealth Asset Index",
           x = "Estimated Wealth Asset Index") +
      theme_minimal() +
      theme(legend.position = c(0.9, 0.1),
            legend.box.background = element_rect(colour = "black"),
            plot.title = element_text(face = "bold", size = 16),
            axis.title = element_text(face = "bold"),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank()) +
      guides(color = guide_legend(override.aes = list(size = 2)),
             alpha = guide_legend(override.aes = list(alpha = 1))) +
      scale_x_continuous(limits = c(min(values), max(values))) +
      scale_y_continuous(limits = c(min(values), max(values)))
    
  } else{
    pred_global_df <- pred_global_df[!is.na(pred_global_df$prediction),]
    
    pred_df_u <- pred_global_df[pred_global_df$urban_rural %in% "U",]
    pred_df_r <- pred_global_df[pred_global_df$urban_rural %in% "R",]
    
    r2_all   <- cor(pred_global_df$truth,   pred_global_df$prediction)^2
    r2_urban <- cor(pred_df_u$truth, pred_df_u$prediction)^2
    r2_rural <- cor(pred_df_r$truth, pred_df_r$prediction)^2
    
    TEXT_X <- -4
    TEXT_Y_TOP <- 4
    TEXT_Y_INCR <- 0.45
    FONT_SIZE <- 4.5
    
    values <- c(pred_global_df$truth, pred_global_df$prediction)
    
    p_list[[p_list_i]] <- ggplot() +
      geom_point(aes(color = urban_rural,
                     y = truth,
                     x = prediction),
                 data = pred_global_df,
                 size = 0.25,
                 alpha = 0.3) +
      geom_richtext(aes(label = paste0("All r<sup>2</sup>: ", round(r2_all,2)),
                        x = TEXT_X,
                        y = TEXT_Y_TOP),
                    color = "black",
                    hjust = 0,
                    fill = NA, 
                    label.color = NA,
                    size = FONT_SIZE) +
      geom_richtext(aes(label = paste0("Urban r<sup>2</sup>: ", round(r2_urban,2)),
                        x = TEXT_X,
                        y = TEXT_Y_TOP - TEXT_Y_INCR),
                    color = "chocolate2",
                    hjust = 0,
                    fill = NA, 
                    label.color = NA,
                    size = FONT_SIZE) + 
      geom_richtext(aes(label = paste0("Rural r<sup>2</sup>: ", round(r2_rural,2)),
                        x = TEXT_X,
                        y = TEXT_Y_TOP - TEXT_Y_INCR*2),
                    color = "chartreuse4",
                    hjust = 0,
                    fill = NA, 
                    label.color = NA,
                    size = FONT_SIZE) +
      scale_color_manual(values = c("chartreuse4", "chocolate2")) +
      labs(color = NULL,
           title = "A. Estimated vs. true wealth index [cluster]",
           y = "True Wealth Asset Index",
           x = "Estimated Wealth Asset Index") +
      theme_minimal() +
      theme(legend.position = c(0.9, 0.1),
            legend.box.background = element_rect(colour = "black"),
            plot.title = element_text(face = "bold", size = 16),
            axis.title = element_text(face = "bold"),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank()) +
      guides(color = guide_legend(override.aes = list(size = 2)),
             alpha = guide_legend(override.aes = list(alpha = 1))) +
      scale_x_continuous(limits = c(min(values), max(values))) +
      scale_y_continuous(limits = c(min(values), max(values)))
    
  }
  
  p_list_i <- p_list_i + 1
  
  # To Long (for map) ------------------------------------------------------------
  cor_df <- pred_best_df %>%
    group_by(country_code, country_name, continent_adj) %>%
    dplyr::summarise(cor = cor(truth, prediction)) %>%
    
    # Change name to match with world shapefile
    dplyr::mutate(country_name = case_when(
      country_name == "Congo - Kinshasa" ~ "Dem. Rep. Congo",
      country_name == "Côte d’Ivoire" ~ "Côte d'Ivoire",
      country_name == "Dominican Republic" ~ "Dominican Rep.",
      country_name == "Myanmar (Burma)" ~ "Myanmar",
      country_name == "Eswatini" ~ "Swaziland",
      TRUE ~ country_name
    )) %>%
    dplyr::mutate(r2 = cor^2)
  
  
  if(aggregate_district){
    cor_mean_all_district <- mean(cor_df$r2)
    cor_mean_africa_district <- mean(cor_df$r2[cor_df$continent_adj %in% "Africa"])
    cor_mean_americas_district <- mean(cor_df$r2[cor_df$continent_adj %in% "Americas"])
    cor_mean_eurasia_district <- mean(cor_df$r2[cor_df$continent_adj %in% "Eurasia"])
  } else{
    cor_mean_all <- mean(cor_df$r2)
    cor_mean_africa <- mean(cor_df$r2[cor_df$continent_adj %in% "Africa"])
    cor_mean_americas <- mean(cor_df$r2[cor_df$continent_adj %in% "Americas"])
    cor_mean_eurasia <- mean(cor_df$r2[cor_df$continent_adj %in% "Eurasia"])
  }
  
  cor_df$r2[cor_df$r2 <= 0.3] <- 0.3
  
  # Map --------------------------------------------------------------------------
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
  
  #cn <- survey_df$country_name %>% unique()
  #cn[!(cn %in% world_sp$country_name)]
  #world_sp$country_name %>% unique()
  
  world_sp <- merge(world_sp, cor_df, by = "country_name", all.x = T, all.y = F)
  
  world_sp$id <- row.names(world_sp)
  world_sp_tidy <- tidy(world_sp)
  world_sp_tidy <- merge(world_sp_tidy, world_sp@data)
  
  TEXT_X_MAP <- -73
  TEXT_Y_TOP_MAP <- 40
  TEXT_Y_INC_MAP <- 4
  
  if(aggregate_district){
    
    p_list[[p_list_i]] <- ggplot() +
      geom_polygon(data = world_sp_tidy[is.na(world_sp_tidy$cor),],
                   aes(x = long, y = lat, group = group),
                   fill = "gray50",
                   color = "white") +
      #geom_polygon(data = world_sp_tidy[!is.na(world_sp_tidy$cor),],
      #             aes(x = long, y = lat, group = group),
      #             color = "black",
      #             fill = "black",
      #             size = 1) +
      geom_polygon(data = world_sp_tidy[!is.na(world_sp_tidy$cor),],
                   aes(x = long, y = lat, group = group,
                       fill = r2),
                   color = "black") +
      geom_richtext(aes(label = paste0("All - Avg r<sup>2</sup>: ", round(cor_mean_all_district,2)),
                        x = TEXT_X_MAP,
                        y = TEXT_Y_TOP_MAP),
                    color = "black",
                    hjust = 0,
                    fill = NA, 
                    label.color = NA,
                    size = FONT_SIZE) +
      geom_richtext(aes(label = paste0("Africa - Avg r<sup>2</sup>: ", round(cor_mean_africa_district,2)),
                        x = TEXT_X_MAP,
                        y = TEXT_Y_TOP_MAP - TEXT_Y_INC_MAP),
                    color = "black",
                    hjust = 0,
                    fill = NA, 
                    label.color = NA,
                    size = FONT_SIZE) +
      geom_richtext(aes(label = paste0("Americas - Avg r<sup>2</sup>: ", round(cor_mean_americas_district,2)),
                        x = TEXT_X_MAP,
                        y = TEXT_Y_TOP_MAP - TEXT_Y_INC_MAP*2),
                    color = "black",
                    hjust = 0,
                    fill = NA, 
                    label.color = NA,
                    size = FONT_SIZE) +
      geom_richtext(aes(label = paste0("Eurasia - Avg r<sup>2</sup>: ", round(cor_mean_eurasia_district,2)),
                        x = TEXT_X_MAP,
                        y = TEXT_Y_TOP_MAP - TEXT_Y_INC_MAP*3),
                    color = "black",
                    hjust = 0,
                    fill = NA, 
                    label.color = NA,
                    size = FONT_SIZE) +
      scale_fill_distiller(palette = "Spectral",
                           direction = 0,
                           limits = c(0.3, 0.949),
                           #values = c(0.3, 0.4, 0.5, 0.6, 0.7, 0.8),
                           labels = c("<0.3", "0.4", "0.5", "0.6", "0.7", "0.8", "0.9")) +
      labs(fill = expression(r^2),
           title = ifelse(aggregate_district,
                          "D. r2 of estimated vs true wealth index within countries [district]",
                          "B. r2 of estimated vs true wealth index within countries [cluster]"), 
           caption = "") +
      theme_void() +
      coord_quickmap() +
      coord_cartesian(xlim=c(-85,135),
                      ylim=c(-34, 45)) +
      theme(plot.title = element_text(face = "bold", size = 16),
            legend.position = c(0.05,0.18)) 
    
  } else{
    
    p_list[[p_list_i]] <- ggplot() +
      geom_polygon(data = world_sp_tidy[is.na(world_sp_tidy$cor),],
                   aes(x = long, y = lat, group = group),
                   fill = "gray50",
                   color = "white") +
      #geom_polygon(data = world_sp_tidy[!is.na(world_sp_tidy$cor),],
      #             aes(x = long, y = lat, group = group),
      #             color = "black",
      #             fill = "black",
      #             size = 1) +
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
                           direction = 0,
                           limits = c(0.3, 0.949),
                           #values = c(0.3, 0.4, 0.5, 0.6, 0.7, 0.8),
                           labels = c("<0.3", "0.4", "0.5", "0.6", "0.7", "0.8", "0.9")) +
      labs(fill = expression(r^2),
           title = ifelse(aggregate_district,
                          "D. r2 of estimated vs true wealth index within countries [district]",
                          "B. r2 of estimated vs true wealth index within countries [cluster]"), 
           caption = "") +
      theme_void() +
      coord_quickmap() +
      coord_cartesian(xlim=c(-85,135),
                      ylim=c(-34, 45)) +
      theme(plot.title = element_text(face = "bold", size = 16),
            legend.position = c(0.05,0.18)) 
  }
  
  p_list_i <- p_list_i + 1
  
  # Country Scatterplot ----------------------------------------------------------
  if(aggregate_district){
    
    p_scatter_country <- pred_best_df %>%
      group_by(country_name) %>%
      dplyr::mutate(cor_val = cor(truth, prediction)^2) %>%
      dplyr::mutate(country_name = paste0(country_name, "\nr2: ", 
                                          round(cor_val, 2))) %>%
      ungroup() %>%
      dplyr::mutate(country_name = reorder(country_name, cor_val, FUN = median, .desc =T) %>%
                      fct_rev()) %>%
      ggplot(aes(y = truth,
                 x = prediction),
             color = "black") +
      geom_point(size = 0.4, # 0.25
                 alpha = 0.7) + # 0.3
      scale_color_manual(values = c("chartreuse4", "chocolate1")) +
      labs(color = NULL,
           title = "Predicted vs. True Wealth Scores",
           y = "True Asset Index",
           x = "Predicted Asset Index") +
      theme_minimal() +
      theme(legend.position = "top",
            legend.box.background = element_rect(colour = "black"),
            plot.title = element_text(face = "bold", size = 16),
            strip.text = element_text(face = "bold")) +
      guides(color = guide_legend(override.aes = list(size = 2)),
             alpha = guide_legend(override.aes = list(alpha = 1))) +
      facet_wrap(~country_name)
    
    SCALE = 1.5
    ggsave(p_scatter_country, 
           filename = file.path(figures_global_dir, "r2_scatter_eachcountry_districtagg.png"),
           height = 10*SCALE, 
           width = 8*SCALE)
    
    
  } else{
    p_scatter_country <- pred_best_df %>%
      group_by(country_name) %>%
      dplyr::mutate(cor_val = cor(truth, prediction)^2) %>%
      dplyr::mutate(country_name = paste0(country_name, "\nr2: ", 
                                          round(cor_val, 2))) %>%
      ungroup() %>%
      dplyr::mutate(country_name = reorder(country_name, cor_val, FUN = median, .desc =T) %>%
                      fct_rev()) %>%
      ggplot(aes(y = truth,
                 x = prediction,
                 color = urban_rural)) +
      geom_point(size = 0.4, # 0.25
                 alpha = 0.7) + # 0.3
      scale_color_manual(values = c("chartreuse4", "chocolate1")) +
      labs(color = NULL,
           title = "Predicted vs. True Wealth Scores",
           y = "True Asset Index",
           x = "Predicted Asset Index") +
      theme_minimal() +
      theme(legend.position = "top",
            legend.box.background = element_rect(colour = "black"),
            plot.title = element_text(face = "bold", size = 16),
            strip.text = element_text(face = "bold")) +
      guides(color = guide_legend(override.aes = list(size = 2)),
             alpha = guide_legend(override.aes = list(alpha = 1))) +
      facet_wrap(~country_name)
    
    SCALE = 1.5
    ggsave(p_scatter_country, 
           filename = file.path(figures_global_dir, "r2_scatter_eachcountry.png"),
           height = 10*SCALE, 
           width = 8*SCALE)
  }
  
}

# Arrange/Export ---------------------------------------------------------------
p1 <- ggarrange(p_list[[1]] + theme(legend.position = "none"),
                p_list[[2]],
                ncol = 2,
                widths = c(0.4, 0.6))

p2 <- ggarrange(p_list[[3]] + theme(legend.position = "none"),
                p_list[[4]],
                ncol = 2,
                widths = c(0.4, 0.6))

p <- ggarrange(p1,
               p2,
               ncol = 1)

ggsave(p, filename = file.path(figures_global_dir, "global_scatter_map.png"),
       height = 4.5*2, width = 15) 

