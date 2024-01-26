# Global Scatterplot - Predicted vs True

p_list_i <- 1
p_list <- list()
for(aggregate_district in c(F, T)){
  
  # Merge with select survey variables -------------------------------------------
  survey_df <- readRDS(file.path(data_dir, "DHS", "FinalData", "Merged Datasets",
                                 "survey_alldata_clean_predictions.Rds"))
  
  survey_df <- survey_df %>%
    filter(most_recent_survey %in% T)
  
  # Aggregate --------------------------------------------------------------------
  if(aggregate_district){
    survey_df <- survey_df %>%
      
      #dplyr::filter(!is.na(predict_pca_allvars_mr_best_all)) %>%
      dplyr::filter(!is.na(predict_pca_allvars_mr_global_country_pred_all)) %>%
      
      dplyr::mutate(gadm_uid = paste(gadm_uid, country_code)) %>%
      group_by(gadm_uid, country_code, country_name, continent_adj) %>%
      dplyr::summarise(pca_allvars_mr = mean(pca_allvars_mr),
                       #predict_pca_allvars_mr_best_all = mean(predict_pca_allvars_mr_best_all),
                       predict_pca_allvars_mr_global_country_pred_all = mean(predict_pca_allvars_mr_global_country_pred_all)) %>%
      ungroup()
    
  }
  
  # One dataset per estimation type --------------------------------------------
  #pred_best_df <- survey_df %>%
  # dplyr::rename(truth = pca_allvars_mr,
  #               prediction = predict_pca_allvars_mr_best_all)
  
  pred_global_df <- survey_df %>%
    dplyr::rename(truth = pca_allvars_mr,
                  prediction = predict_pca_allvars_mr_global_country_pred_all)
  
  # Scatterplot ------------------------------------------------------------------
  if(aggregate_district){
    
    r2_all_d   <- cor(pred_global_df$truth,   pred_global_df$prediction)^2
    R2_all_d   <- R2(pred_global_df$prediction, pred_global_df$truth, form = "traditional")
    
    TEXT_X <- -3.75
    TEXT_Y_TOP <- 3
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
      geom_richtext(aes(label = paste0("r<sup>2</sup>: ", round(r2_all_d,2), "; R<sup>2</sup>: ", round(R2_all_d,2)),
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
    
    R2_all   <- R2(pred_global_df$prediction,   pred_global_df$truth, form = "traditional")
    R2_urban <- R2(pred_df_u$prediction, pred_df_u$truth, form = "traditional")
    R2_rural <- R2(pred_df_r$prediction, pred_df_r$truth, form = "traditional")
    
    TEXT_X <- -3.75
    TEXT_Y_TOP <- 3
    TEXT_Y_INCR <- 0.45
    FONT_SIZE <- 4.5
    
    values <- c(pred_global_df$truth, pred_global_df$prediction)
    
    p_list[[p_list_i]] <- ggplot() +
      geom_point(aes(color = urban_rural,
                     x = truth,
                     y = prediction),
                 data = pred_global_df,
                 size = 0.25,
                 alpha = 0.3) +
      geom_richtext(aes(label = paste0("All r<sup>2</sup>: ", round(r2_all,2), "; R<sup>2</sup>: ", round(R2_all, 2)),
                        x = -4.2,
                        y = 3.9),
                    color = "black",
                    hjust = 0,
                    fill = NA, 
                    label.color = NA,
                    size = FONT_SIZE) +
      geom_richtext(aes(label = paste0("Urban r<sup>2</sup>: ", round(r2_urban,2), "; R<sup>2</sup>: ", round(R2_urban,2)),
                        x = -4.2,
                        y = 3.9 - TEXT_Y_INCR),
                    color = "chocolate2",
                    hjust = 0,
                    fill = NA, 
                    label.color = NA,
                    size = FONT_SIZE) + 
      geom_richtext(aes(label = paste0("Rural r<sup>2</sup>: ", round(r2_rural,2), "; R<sup>2</sup>: ", round(R2_rural,2)),
                        x = -4.2,
                        y = 3.9 - TEXT_Y_INCR*2),
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
  cor_df <- pred_global_df %>%
    group_by(country_code, country_name, continent_adj) %>%
    dplyr::summarise(cor = cor(prediction, truth),
                     R2 = R2(prediction, truth, form = "traditional")) %>%
    
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
    
    R2_mean_all_district <- mean(cor_df$R2)
    R2_mean_africa_district <- mean(cor_df$R2[cor_df$continent_adj %in% "Africa"])
    R2_mean_americas_district <- mean(cor_df$R2[cor_df$continent_adj %in% "Americas"])
    R2_mean_eurasia_district <- mean(cor_df$R2[cor_df$continent_adj %in% "Eurasia"])
  } else{
    cor_mean_all <- mean(cor_df$r2)
    cor_mean_africa <- mean(cor_df$r2[cor_df$continent_adj %in% "Africa"])
    cor_mean_americas <- mean(cor_df$r2[cor_df$continent_adj %in% "Americas"])
    cor_mean_eurasia <- mean(cor_df$r2[cor_df$continent_adj %in% "Eurasia"])
    
    R2_mean_all <- mean(cor_df$R2)
    R2_mean_africa <- mean(cor_df$R2[cor_df$continent_adj %in% "Africa"])
    R2_mean_americas <- mean(cor_df$R2[cor_df$continent_adj %in% "Americas"])
    R2_mean_eurasia <- mean(cor_df$R2[cor_df$continent_adj %in% "Eurasia"])
  }
  
  #cor_df$r2[cor_df$r2 <= 0.3] <- 0.3
  
  # Map --------------------------------------------------------------------------
  world_sp <- ne_countries(type = "countries", scale=50) %>% as("Spatial")
  
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
      geom_polygon(data = world_sp_tidy[!is.na(world_sp_tidy$cor),],
                   aes(x = long, y = lat, group = group,
                       fill = r2),
                   color = "black") +
      geom_richtext(aes(label = paste0("All r<sup>2</sup>: ", round(cor_mean_all_district,2), "; R<sup>2</sup>: ", round(R2_mean_all_district,2)),
                        x = TEXT_X_MAP,
                        y = TEXT_Y_TOP_MAP),
                    color = "black",
                    hjust = 0,
                    fill = NA, 
                    label.color = NA,
                    size = FONT_SIZE) +
      geom_richtext(aes(label = paste0("Africa r<sup>2</sup>: ", round(cor_mean_africa_district,2), "; R<sup>2</sup>: ", round(R2_mean_africa_district,2)),
                        x = TEXT_X_MAP,
                        y = TEXT_Y_TOP_MAP - TEXT_Y_INC_MAP),
                    color = "black",
                    hjust = 0,
                    fill = NA, 
                    label.color = NA,
                    size = FONT_SIZE) +
      geom_richtext(aes(label = paste0("Americas r<sup>2</sup>: ", round(cor_mean_americas_district,2), "; R<sup>2</sup>: ", round(R2_mean_americas_district,2)),
                        x = TEXT_X_MAP,
                        y = TEXT_Y_TOP_MAP - TEXT_Y_INC_MAP*2),
                    color = "black",
                    hjust = 0,
                    fill = NA, 
                    label.color = NA,
                    size = FONT_SIZE) +
      geom_richtext(aes(label = paste0("Eurasia r<sup>2</sup>: ", round(cor_mean_eurasia_district,2), "; R<sup>2</sup>: ", round(R2_mean_eurasia_district,2)),
                        x = TEXT_X_MAP,
                        y = TEXT_Y_TOP_MAP - TEXT_Y_INC_MAP*3),
                    color = "black",
                    hjust = 0,
                    fill = NA, 
                    label.color = NA,
                    size = FONT_SIZE) +
      scale_fill_distiller(palette = "Spectral",
                           direction = 0,
                           limits = c(0, 1)) +
                           #labels = c("0.1", "0.3", "0.4", "0.5", "0.6", "0.7", "0.8", "0.9")
                           #values = c(0.3, 0.4, 0.5, 0.6, 0.7, 0.8)) +
      labs(fill = expression(r^2),
           title = ifelse(aggregate_district,
                          "D. r2 of estimated vs true wealth index within countries [district]",
                          "B. r2 of estimated vs true wealth index within countries [cluster]"), 
           caption = "") +
      theme_void() +
      coord_quickmap() +
      coord_cartesian(xlim=c(-90,135),
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
      geom_richtext(aes(label = paste0("All r<sup>2</sup>: ", round(cor_mean_all,2), "; R<sup>2</sup>: ", round(R2_mean_all,2)),
                        x = TEXT_X_MAP,
                        y = TEXT_Y_TOP_MAP),
                    color = "black",
                    hjust = 0,
                    fill = NA, 
                    label.color = NA,
                    size = FONT_SIZE) +
      geom_richtext(aes(label = paste0("Africa r<sup>2</sup>: ", round(cor_mean_africa,2), "; R<sup>2</sup>: ", round(R2_mean_africa,2)),
                        x = TEXT_X_MAP,
                        y = TEXT_Y_TOP_MAP - TEXT_Y_INC_MAP),
                    color = "black",
                    hjust = 0,
                    fill = NA, 
                    label.color = NA,
                    size = FONT_SIZE) +
      geom_richtext(aes(label = paste0("Americas r<sup>2</sup>: ", round(cor_mean_americas,2), "; R<sup>2</sup>: ", round(R2_mean_americas,2)),
                        x = TEXT_X_MAP,
                        y = TEXT_Y_TOP_MAP - TEXT_Y_INC_MAP*2),
                    color = "black",
                    hjust = 0,
                    fill = NA, 
                    label.color = NA,
                    size = FONT_SIZE) +
      geom_richtext(aes(label = paste0("Eurasia r<sup>2</sup>: ", round(cor_mean_eurasia,2), "; R<sup>2</sup>: ", round(R2_mean_eurasia,2)),
                        x = TEXT_X_MAP,
                        y = TEXT_Y_TOP_MAP - TEXT_Y_INC_MAP*3),
                    color = "black",
                    hjust = 0,
                    fill = NA, 
                    label.color = NA,
                    size = FONT_SIZE) +
      scale_fill_distiller(palette = "Spectral",
                           direction = 0,
                           limits = c(0,1)) +
                           #limits = c(0.3, 0.949), # 0.3, 0.949
                           #values = c(0.3, 0.4, 0.5, 0.6, 0.7, 0.8),
                           #labels = c("<0.3", "0.4", "0.5", "0.6", "0.7", "0.8", "0.9")) +
      labs(fill = expression(r^2),
           title = ifelse(aggregate_district,
                          "D. r2 of estimated vs true wealth index within countries [district]",
                          "B. r2 of estimated vs true wealth index within countries [cluster]"), 
           caption = "") +
      theme_void() +
      coord_quickmap() +
      coord_cartesian(xlim=c(-90,135),
                      ylim=c(-34, 45)) +
      theme(plot.title = element_text(face = "bold", size = 16),
            legend.position = c(0.05,0.18)) 
  }
  
  p_list_i <- p_list_i + 1
  
  # Country Scatterplot ----------------------------------------------------------
  if(aggregate_district){
    
    p_scatter_country <- pred_global_df %>%
      group_by(country_name) %>%
      
      dplyr::mutate(cor_val = cor(truth, prediction)^2,
                    coef_det_val = R2(prediction, truth, form = "traditional")) %>%
      dplyr::mutate(country_name = paste0(country_name, 
                                          "\nr2: ", round(cor_val, 2),
                                          "\nR2: ", round(coef_det_val, 2))) %>%
      
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
           title = "Estimated vs. True Wealth Scores",
           y = "True Wealth Asset Index",
           x = "Estimated Wealth Asset Index") +
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
    
    p_scatter_country <- pred_global_df %>%
      group_by(country_name) %>%
      dplyr::mutate(cor_val = cor(truth, prediction)^2,
                    coef_det_val = R2(prediction, truth, form = "traditional")) %>%
      dplyr::mutate(country_name = paste0(country_name, 
                                          "\nr2: ", round(cor_val, 2),
                                          "\nR2: ", round(coef_det_val, 2))) %>%
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
           title = "Estimated vs. True Wealth Scores",
           y = "True Wealth Asset Index",
           x = "Estimated Wealth Asset Index") +
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

