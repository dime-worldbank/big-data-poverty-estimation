# Changes
# Scatterplot Each Country

# TODO: May need to fix to be consistent with other figure?

# Load data --------------------------------------------------------------------
for(unit in c("cluster", "adm2")){
  for(est_type in c("within_country_cv", "same_continent")){
    survey_df <- readRDS(file.path(data_dir, SURVEY_NAME, "FinalData", "Merged Datasets",
                                   "survey_alldata_clean_changes_cluster_predictions.Rds"))
    
    if(unit == "adm2"){
      survey_df <- survey_df %>%
        group_by(continent_adj, country_code, country_name, gadm_uid) %>%
        summarise_if(is.numeric, mean) %>%
        ungroup()
    }
    
    survey_df$predict_var <- survey_df[[paste0("predict_pca_allvars_",est_type,"_all_changes")]]
    
    # Figure -----------------------------------------------------------------------
    p_pca <- survey_df %>%
      group_by(country_name) %>%
      dplyr::mutate(r2 = cor(pca_allvars,
                             predict_var)^2) %>%
      ungroup() %>%
      dplyr::mutate(country_name = fct_reorder(country_name,
                                               -r2)) %>%
      ggplot(aes(x = pca_allvars,
                 y = predict_var)) +
      geom_point(size = 0.3) +
      stat_poly_eq(color = "firebrick3") +
      facet_wrap(~country_name) +
      theme_classic() +
      labs(x = "True Asset Index",
           y = "Predicted Asset Index") +
      theme(strip.background = element_blank(),
            strip.text = element_text(face = "bold"))
    
    # p_dhs <- survey_df %>%
    #   group_by(country_name) %>%
    #   dplyr::mutate(r2 = cor(wealth_index_score,
    #                          predict_var)^2) %>%
    #   ungroup() %>%
    #   dplyr::mutate(country_name = fct_reorder(country_name,
    #                                            -r2)) %>%
    #   ggplot(aes(x = wealth_index_score,
    #              y = predict_var)) +
    #   geom_point(size = 0.3) +
    #   stat_poly_eq(color = "firebrick3") +
    #   facet_wrap(~country_name) +
    #   theme_classic() +
    #   labs(x = "True DHS Wealth Index",
    #        y = "Predicted DHS Wealth Index") +
    #   theme(strip.background = element_blank(),
    #         strip.text = element_text(face = "bold"))
    
    ggsave(p_pca,
           filename = file.path(figures_global_dir, paste0("ml_changes_scatter_eachcountry_pca_",unit,"_",est_type,".png")),
           height = 12,
           width = 12)
    
    # ggsave(p_dhs,
    #        filename = file.path(figures_global_dir, paste0("ml_changes_scatter_eachcountry_dhs_",unit,"_",est_type,".png")),
    #        height = 12,
    #        width = 12)
    # 
  }
}

