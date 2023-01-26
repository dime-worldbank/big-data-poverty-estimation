# Create bins for nighttime lights

# For CNN: Data after 2000, as landsat 7, but still predict beforehand

set.seed(4242)
N_BINS <- 5
N_OBS_IN_TFR <- 250
TEST_PROP <- 0.2
INDIA_UNDER_SAMPLE <- T
DTL_SATELLITE <- "s2"

for(DTL_SATELLITE in c("landsat", "s2")){
  for(INDIA_UNDER_SAMPLE in c(T, F)){
    
    # Load data --------------------------------------------------------------------
    survey_all_df <- readRDS(file.path(data_dir, SURVEY_NAME, "FinalData", 
                                       "Individual Datasets",
                                       "survey_socioeconomic.Rds"))
    
    if(DTL_SATELLITE %in% "s2"){
      # For s2, we always just extract 2018 - 2020 data
      ntl_df <- readRDS(file.path(data_dir, SURVEY_NAME, "FinalData", "Individual Datasets",
                                  "satellite_data_from_gee", 
                                  "viirs181920_1120_ubuff1120_rbuff1120.Rds"))
    } else if (DTL_SATELLITE %in% "landsat"){
      ntl_df <- readRDS(file.path(data_dir, SURVEY_NAME, "FinalData", "Individual Datasets",
                                  "satellite_data_from_gee", 
                                  "viirs_3360_ubuff3360_rbuff3360.Rds"))
    }
    
    survey_all_df <- survey_all_df %>%
      left_join(ntl_df, by = c("uid", "year")) %>%
      dplyr::filter(!is.na(avg_rad)) %>%
      arrange(runif(n()))
    
    if(SURVEY_NAME %in% "DHS"){
      years <- "all"
    } else if(SURVEY_NAME %in% "OPM"){
      years <- unique(survey_all_df$year)
    }
    
    survey_df_clean_append <- map_df(years, function(year_i){
      print(year_i)
      
      if(year_i == "all"){
        survey_df <- survey_all_df
      } else{
        survey_df <- survey_all_df[survey_all_df$year %in% year_i,]
      }
      
      # Create bins ----------------------------------------------------------------
      mclust_fit = Mclust(survey_df$avg_rad, G=N_BINS, model="V")
      survey_df$ntl_group <- predict(mclust_fit, survey_df$avg_rad)$classification
      
      # Create balanced groups -------------------------------------------------------
      survey_df <- survey_df %>%
        arrange(runif(n()))
      
      ## To undersample India
      survey_df$keep <- 1
      
      if(INDIA_UNDER_SAMPLE){
        survey_df$keep[survey_df$country_code %in% "IA"] <- sample(
          x = c(0,1),
          size = length(survey_df$keep[survey_df$country_code %in% "IA"]),
          replace = T,
          prob = c(0.9, 0.1)
        )
      }  
      
      survey_df_recent <- survey_df %>%
        dplyr::filter(year >= 2012,
                      keep %in% T,
                      most_recent_survey %in% T)
      
      min_group_size <- survey_df_recent$ntl_group %>%
        table() %>%
        as.data.frame() %>%
        pull(Freq) %>% 
        min()
      
      ### Split into groups
      # (1) For CNN - train
      # (2) For CNN - test
      # (3) For CNN - validation
      
      # Create survey with balanced groups
      survey_df_forcnn <- map_df(unique(survey_df_recent$ntl_group), function(i){
        survey_df_gi <- survey_df_recent[survey_df_recent$ntl_group %in% i,]
        return(survey_df_gi[1:min_group_size,])
      })
      
      # For cnn - train/test
      n_test_bin <- floor(nrow(survey_df_forcnn)/N_BINS * TEST_PROP)
      
      survey_df_forcnn_test <- map_df(unique(survey_df_forcnn$ntl_group), function(i){
        survey_df_forcnn_gi <- survey_df_forcnn[survey_df_forcnn$ntl_group %in% i,]
        return(survey_df_forcnn_gi[1:n_test_bin,])
      })
      
      survey_df_forcnn_train <- survey_df_forcnn[!(survey_df_forcnn$uid %in% survey_df_forcnn_test$uid),]
      
      # Observations not used for cnn
      survey_df_nocnn <- survey_df[!(survey_df$uid %in% survey_df_forcnn$uid),]
      #survey_df_nocnn <- bind_rows(survey_df_nocnn,
      #                             survey_df_old)
      
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
      
      survey_df_forcnn_test_tf <- make_tfrecord_name(survey_df_forcnn_test, "forcnn_test", N_OBS_IN_TFR) %>%
        dplyr::mutate(use_for_cnn = "yes",
                      traintest = "test")
      survey_df_forcnn_train_tf <- make_tfrecord_name(survey_df_forcnn_train, "forcnn_train", N_OBS_IN_TFR) %>%
        dplyr::mutate(use_for_cnn = "yes",
                      traintest = "train")
      survey_df_nocnn_tf <- make_tfrecord_name(survey_df_nocnn, "nocnn", N_OBS_IN_TFR) %>%
        dplyr::mutate(use_for_cnn = "no")
      
      survey_df_clean <- bind_rows(survey_df_forcnn_test_tf,
                                   survey_df_forcnn_train_tf,
                                   survey_df_nocnn_tf)
      
      # Export -----------------------------------------------------------------------
      survey_df_clean <- survey_df_clean %>%
        dplyr::select(uid, GID_2, year, most_recent_survey, ntl_group, longitude, latitude, tfrecord_name, use_for_cnn) %>%
        
        # So starts at 0; better for python
        dplyr::mutate(ntl_group = ntl_group - 1)
      
      return(survey_df_clean)
      
    })
    
    write.csv(survey_df_clean_append, file.path(data_dir, SURVEY_NAME, "FinalData",
                                                "Individual Datasets",
                                                paste0("data_for_cnn_viirsgee_iaunder",INDIA_UNDER_SAMPLE,"_",DTL_SATELLITE,".csv")),
              row.names = F)
    
    
    # write.csv(survey_df_clean_append, file.path(gdrive_dir,
    #                                             "Data", SURVEY_NAME, "FinalData",
    #                                             "Individual Datasets",
    #                                             paste0("data_for_cnn_viirs_iaunder",INDIA_UNDER_SAMPLE,"_",DTL_SATELLITE,".csv")),
    #           row.names = F)
    
  }
}


survey_df_clean_append$c <- survey_df_clean_append$uid %>% substring(1,2)
cnn_df <- survey_df_clean_append[survey_df_clean_append$tfrecord_name %>% str_detect("forcnn"),]
cnn_df$c %>% table()

survey_df_clean_append$ntl_group %>% table()
survey_df_clean_append$ntl_group[survey_df_clean_append$tfrecord_name %>% str_detect("forcnn")] %>% table()
survey_df_clean_append$ntl_group[survey_df_clean_append$tfrecord_name %>% str_detect("forcnn_train")] %>% table()
survey_df_clean_append$ntl_group[survey_df_clean_append$tfrecord_name %>% str_detect("forcnn_test")] %>% table()

cnn_all <- survey_df_clean_append$tfrecord_name %>% 
  str_detect("forcnn") %>% sum()
cnn_train <- survey_df_clean_append$tfrecord_name %>% 
  str_detect("forcnn_train") %>% sum()
cnn_test <- survey_df_clean_append$tfrecord_name %>% 
  str_detect("forcnn_test") %>% sum()

cnn_train/cnn_all
cnn_test/cnn_all

survey_df_clean_append$uid %>% table %>% table()
