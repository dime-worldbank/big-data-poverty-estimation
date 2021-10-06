# Create bins for nighttime lights

set.seed(4242)

# Load data --------------------------------------------------------------------
survey_all_df <- readRDS(file.path(data_dir, SURVEY_NAME, "FinalData", 
                                   "Individual Datasets",
                                   "survey_socioeconomic.Rds"))

viirs_df <- readRDS(file.path(data_dir, SURVEY_NAME, "FinalData", "Individual Datasets",
                              "satellite_data_from_gee", 
                              "viirs_ubuff2500_rbuff2500.Rds"))

survey_all_df <- survey_all_df %>%
  left_join(viirs_df, by = c("uid", "year")) %>%
  dplyr::filter(!is.na(avg_rad)) %>%
  arrange(runif(n()))

if(SURVEY_NAME %in% "DHS"){
  years <- "all"
} else if(SURVEY_NAME %in% "OPM"){
  years <- unique(survey_all_df$year)
}

survey_df_clean_append <- map_df(years, function(year_i){
  print(year_i)
  
  if(year_i != "all"){
    survey_df <- survey_all_df[survey_all_df$year %in% year_i,]
  } else{
    survey_df <- survey_all_df
  }

  # Create bins ------------------------------------------------------------------
  mclust_fit = Mclust(survey_df$avg_rad, G=3, model="V")
  survey_df$ntl_group <- predict(mclust_fit, survey_df$avg_rad)$classification
  
  # Create balanced groups -------------------------------------------------------
  min_group_size <- survey_df$ntl_group %>%
    table() %>%
    as.data.frame() %>%
    pull(Freq) %>% 
    min()
  
  survey_df <- survey_df %>%
    arrange(runif(n()))
  
  # Create survey with balanced groups
  survey_df_forcnn <- map_df(unique(survey_df$ntl_group), function(i){
    survey_df_gi <- survey_df[survey_df$ntl_group %in% i,]
    return(survey_df_gi[1:min_group_size,])
  })
  
  survey_df_nocnn <- survey_df[!(survey_df$uid %in% survey_df_forcnn$uid),]
  
  # Add TFRecord Name ------------------------------------------------------------
  # Folder name for storing individual tfrecords. Google drive complains when
  # too many files in an individual folder. Ensure names separate across groups
  # survey for CNN and for those not for CNN (to ensure balanced sample).
  
  make_tfrecord_name <- function(dhs_all_df_coll,
                                 prefix,
                                 N_OBS_IN_TFR = 100){
    
    dhs_all_df_coll$tfrecord_name <- ""
    
    for(fold_name in unique(dhs_all_df_coll$within_country_fold)){
      N_in_fold <- sum(dhs_all_df_coll$within_country_fold %in% fold_name)
      
      tfrecord_id <- rep(1:10000, times=1, each=N_OBS_IN_TFR)[1:N_in_fold]
      tfrecord_id <- sample(tfrecord_id) # Randomly sort
      
      tfrecord_name <- paste0(prefix, "_", fold_name, "_", tfrecord_id, "_", year_i, ".tfrecord")
      
      dhs_all_df_coll$tfrecord_name[dhs_all_df_coll$within_country_fold %in% fold_name] <- tfrecord_name
    }
    
    return(dhs_all_df_coll)
  }
  
  survey_df_forcnn_tf <- make_tfrecord_name(survey_df_forcnn, "forcnn") %>%
    dplyr::mutate(use_for_cnn = "yes")
  survey_df_nocnn_tf <- make_tfrecord_name(survey_df_nocnn, "nocnn") %>%
    dplyr::mutate(use_for_cnn = "no")
  
  survey_df_clean <- bind_rows(survey_df_forcnn_tf,
                               survey_df_nocnn_tf)
  
  # Export -----------------------------------------------------------------------
  survey_df_clean <- survey_df_clean %>%
    dplyr::select(uid, GID_2, year, ntl_group, longitude, latitude, tfrecord_name, use_for_cnn) %>%
    
    # So starts at 0; better for python
    dplyr::mutate(ntl_group = ntl_group - 1)
  
  return(survey_df_clean)
  
})

write.csv(survey_df_clean_append, file.path(data_dir, SURVEY_NAME, "FinalData",
                                     "Individual Datasets",
                                     "data_for_cnn.csv"),
          row.names = F)


write.csv(survey_df_clean_append, file.path(gdrive_dir,
                                     "Data", SURVEY_NAME, "FinalData",
                                     "Individual Datasets",
                                     "data_for_cnn.csv"),
          row.names = F)




