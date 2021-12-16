# Merge Data Extracted from Facebook API

# Load Data --------------------------------------------------------------------
i <<- 1

# Use data.table rbindlist to efficiently append
# https://rstudio-pubs-static.s3.amazonaws.com/406521_7fc7b6c1dc374e9b8860e15a699d8bb0.html
df_long <- file.path(data_dir, SURVEY_NAME, "FinalData", "Individual Datasets", 
                     "fb_mau_individual_datasets") %>%
  list.files(pattern = "*.Rds",
             full.names = T) %>%
  lapply(function(path){
    df <- readRDS(path) 
    
    # If there was an error, example incorrect location, won't have uid
    if(is.null(df$uid)){
      df_out <- data.frame(NULL) %>%
        as.data.table()
    } else{
      df_out <- df %>%
        dplyr::select(uid, param_id, estimate_dau, estimate_mau) %>%
        dplyr::mutate(uid = uid %>% as.character()) %>%
        as.data.table()
    }
    
    i <<- i + 1
    if((i %% 100) %in% 0) print(paste0("Append FB: ", i))
    
    return(df_out)  
  }) %>%
  rbindlist() %>%
  as.data.frame()
rm(i)

# To Wide ----------------------------------------------------------------------
df_wide <- df_long %>%
  pivot_wider(id_cols = c(uid),
              names_from = param_id,
              values_from = c(estimate_dau, estimate_mau))

# To Proportion ----------------------------------------------------------------
# estimate_[mau/dau]_1 is all facebook users, so divide by this to 
# get proportion of facebook users that meet a certain category

df_wide_prop <- df_wide %>%
  dplyr::mutate_at(vars(contains("dau"), -estimate_dau_1), ~(. /  estimate_dau_1)) %>%
  dplyr::mutate_at(vars(contains("mau"), -estimate_mau_1), ~(. /  estimate_mau_1)) %>%
  dplyr::select(-c(estimate_dau_1, estimate_mau_1)) %>%
  dplyr::mutate_if(is.numeric, tidyr::replace_na, 0)

# Export -----------------------------------------------------------------------
saveRDS(df_wide, file.path(data_dir, SURVEY_NAME, "FinalData", "Individual Datasets", "facebook_marketing_dau_mau.Rds"))
write.csv(df_wide, file.path(data_dir, SURVEY_NAME, "FinalData", "Individual Datasets", "facebook_marketing_dau_mau.csv"), row.names = F)

saveRDS(df_wide_prop, file.path(data_dir, SURVEY_NAME, "FinalData", "Individual Datasets", "facebook_marketing_dau_mau_prop.Rds"))
write.csv(df_wide_prop, file.path(data_dir, SURVEY_NAME, "FinalData", "Individual Datasets", "facebook_marketing_dau_mau_prop.csv"), row.names = F)










