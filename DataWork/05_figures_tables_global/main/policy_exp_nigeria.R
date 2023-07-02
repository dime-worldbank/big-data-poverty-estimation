# Policy Experiment: Nigeria

for(gadm in c("gadm_1", "gadm_2")){
  
  # Load data ------------------------------------------------------------------
  dhs_df <- readRDS(file.path(data_dir, "DHS_nga_policy_experiment", "FinalData", 
                              "Merged Datasets", "survey_alldata.Rds"))
  
  if(gadm == "gadm_1") dhs_df$GID_2 <- dhs_df$GID_1
  
  # Add wealth estimate ----------------------------------------------------------
  model_use <- file.path(data_dir, "DHS", "FinalData", "pov_estimation_results", "models") %>%
    list.files() %>% 
    str_subset("levels_changevars_ng") %>%
    str_subset("xgboost") %>%
    str_subset("global_country")
  
  ml_model <- readRDS(file.path(data_dir, "DHS", "FinalData", "pov_estimation_results", "models",
                                model_use))
  
  X <- dhs_df %>%
    dplyr::select_at(vars(starts_with("ntlharmon_"),
                          starts_with("cnn_ntlharmon_landsat_"),
                          starts_with("l7_"),
                          starts_with("gc_"),
                          starts_with("weather_"),
                          starts_with("pollution_aod_"))) %>%
    as.matrix()
  
  dhs_df$pca_allvars_predict <- predict(ml_model, X)
  
  # OOS DHS Estimates ------------------------------------------------------------
  #### Make GADM Estimate
  gadm_long_df <- dhs_df %>%
    group_by(year, GID_2) %>%
    dplyr::summarise(pca_allvars_predict = mean(pca_allvars_predict),
                     pca_allvars = mean(pca_allvars)) %>%
    ungroup() %>%
    
    group_by(GID_2) %>%
    mutate(n_year_per_gid = n()) %>%
    ungroup() %>%
    dplyr::filter(n_year_per_gid == 4) %>%
    arrange(year)
  
  #### Function for interpolating/extrapolating
  linear_function <- function(year, df, dhs_year_1, dhs_year_2){
    
    ## Calculate Poverty
    df_i <- df %>%
      dplyr::filter(year %in% c(dhs_year_1, dhs_year_2))
    
    m         <- (df_i$pca_allvars_predict[2] - df_i$pca_allvars_predict[1]) / (df_i$year[2] - df_i$year[1])
    b         <- df_i$pca_allvars_predict[1]
    base_year <- df_i$year[1]
    
    year_diff <- (year - df_i$year[1])
    
    pov = m*year_diff + df_i$pca_allvars_predict[1]
    
    ## Output
    df_out <- data.frame(
      year = year,
      pca_dhs_oos = pov,
      pca_ml_predict = df$pca_allvars_predict[df$year == year],
      pca_true = df$pca_allvars[df$year == year],
      dhs_year_1 = dhs_year_1,
      dhs_year_2 = dhs_year_2
    )
    
    return(df_out)
  }
  
  ## OOS Estimates
  oos_df <- map_df(unique(gadm_long_df$GID_2), function(gid_i){
    
    gadm_long_df_i <- gadm_long_df[gadm_long_df$GID_2 %in% gid_i,]
    
    df_out <- bind_rows(
      linear_function(2003, gadm_long_df_i, 2008, 2013) %>% mutate(method = "Extrapolate", survey_scenerio = "Frequent", title = "Extrapolate\n2003 using\n2008 and 2013 data"),
      linear_function(2003, gadm_long_df_i, 2008, 2018) %>% mutate(method = "Extrapolate", survey_scenerio = "Sparse", title = "Extrapolate\n2003 using\n2008 and 2013 data"),
      
      linear_function(2008, gadm_long_df_i, 2003, 2013) %>% mutate(method = "Interpolate", survey_scenerio = "Frequent", title = "Interpolate\n2008 using\n2003 and 2013 data"),
      linear_function(2008, gadm_long_df_i, 2003, 2018) %>% mutate(method = "Interpolate", survey_scenerio = "Sparse", title = "Interpolate\n2008 using\n2003 and 2018 data"),
      
      linear_function(2013, gadm_long_df_i, 2008, 2018) %>% mutate(method = "Interpolate", survey_scenerio = "Frequent", title = "Interpolate\n2013 using\n2008 and 2018 data"),
      linear_function(2013, gadm_long_df_i, 2003, 2018) %>% mutate(method = "Interpolate", survey_scenerio = "Sparse", title = "Interpolate\n2013 using\n2003 and 2018 data"),
      
      linear_function(2018, gadm_long_df_i, 2003, 2013) %>% mutate(method = "Extrapolate", survey_scenerio = "Frequent", title = "Extrapolate\n2018 using\n2008 and 2013 data"),
      linear_function(2018, gadm_long_df_i, 2008, 2013) %>% mutate(method = "Extrapolate", survey_scenerio = "Sparse", title = "Extrapolate\n2018 using\n2008 and 2013 data")
    )
    
    df_out$GID_2 <- gid_i
    
    return(df_out)
  })
  
  # Figure -----------------------------------------------------------------------
  oos_long_df <- oos_df %>%
    dplyr::select(year, pca_dhs_oos, pca_ml_predict, pca_true, title, survey_scenerio, GID_2) %>%
    pivot_longer(cols = -c(title, survey_scenerio, GID_2, pca_true, year)) %>%
    dplyr::filter(survey_scenerio == "Frequent") %>%
    mutate(name = case_when(
      name == "pca_ml_predict" ~ "Estimate from\nML Model",
      name == "pca_dhs_oos" ~ "DHS Interpolation/\nExtrapolation",
    ) %>% fct_rev()) 
  
  #limit <- c(oos_long_df$pca_true, oos_long_df$value) %>% abs() %>% max()
  limit <- 4
  
  r2_R2_df <- oos_long_df %>%
    group_by(year, name) %>%
    dplyr::summarise(r2 = cor(value, pca_true)^2,
                     R2 = R2(value, pca_true, form = "traditional")) %>%
    ungroup() %>%
    mutate(label = paste0("r<sup>2</sup>: ", round(r2, 2), "<br>R<sup>2</sup>: ", round(R2, 2)))
  
  p <- oos_long_df %>%
    ggplot(aes(x = value, y = pca_true)) +
    
    geom_richtext(data = r2_R2_df,
                  aes(x = -3.25, y = 2.75, label = label),
                  fill = NA, 
                  label.color = NA,
                  size = 2.5,
                  color = "red") +
    
    geom_point(size = 0.25) +
    geom_smooth(method = lm, se = F, color = "darkorange") +
    
    # stat_cor(aes(label = paste(gsub("R", "r", ..rr.label..))),
    #          label.x.npc = "left",
    #          color = "red",
    #          size = 3) +
    facet_grid(year~name) +
    theme_classic() +
    theme(strip.background = element_blank(),
          strip.text = element_text(face = "bold"),
          plot.title = element_text(face = "bold", size = 10)) +
    scale_x_continuous(limits = c(-limit, limit)) +
    scale_y_continuous(limits = c(-limit, limit)) +
    labs(x = "Estimated Wealth",
         y = "True Wealth")
  
  ggsave(p,
         filename = file.path(figures_global_dir, paste0("policy_exp_nigeria_",gadm,".png")),
         height = 5,
         width = 4)
  
}
