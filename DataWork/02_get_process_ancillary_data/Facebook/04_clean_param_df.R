# Merge Data Extracted from Facebook API

# Load Data ----------------------------------------------------------------------
SURVEY_NAME = 'DHS'
param_df <- read.csv(file.path(data_dir, SURVEY_NAME, "FinalData", "Individual Datasets", "facebook_marketing_parameters.csv"), stringsAsFactors = F)

behaviors_df <- read.csv(file.path(data_dir, "Facebook", "FinalData", "interests_demographics_behaviors_ids", "behaviors.csv"), stringsAsFactors = F)
interests_df <- read.csv(file.path(data_dir, "Facebook", "FinalData", "interests_demographics_behaviors_ids", "interests.csv"), stringsAsFactors = F)

# Add behavior/interests names -------------------------------------------------
behaviors_df <- behaviors_df %>% mutate(id = id %>% as.character())
interests_df <- interests_df %>% mutate(id = id %>% as.character())

param_df <- param_df %>%
  dplyr::mutate(behavior = behavior %>% str_replace_all("'id'|\\{|:|\\}", ""),
                interest = interest %>% str_replace_all("'id'|\\{|:|\\}", ""))

beh_int_df <- map_df(1:nrow(param_df), function(i){
  
  param_df_i <- param_df[i,]
  
  beh_ids_i <- param_df_i$behavior %>% str_split(",") %>% unlist()
  beh_name_i <- behaviors_df$name[behaviors_df$id %in% beh_ids_i] %>% paste(collapse = "; ")
  
  int_ids_i <- param_df_i$interest %>% str_split(",") %>% unlist()
  int_name_i <- interests_df$name[interests_df$id %in% int_ids_i] %>% paste(collapse = "; ")
  
  out <- data.frame(behavior_name = beh_name_i,
                    interest_name = int_name_i)
  
  return(out)
})

param_df <- bind_cols(param_df, beh_int_df) %>%
  dplyr::mutate(behavior_name = behavior_name %>% as.character,
                interest_name = interest_name %>% as.character)
param_df$behavior_name[param_df$behavior_name %in% ""] <- NA
param_df$interest_name[param_df$interest_name %in% ""] <- NA

# Edit Eduction ----------------------------------------------------------------
# https://developers.facebook.com/docs/marketing-api/audiences/reference/advanced-targeting/#education_and_workplace
# 1: HIGH_SCHOOL
# 2: UNDERGRAD
# 3: ALUM
# 4: HIGH_SCHOOL_GRAD
# 5: SOME_COLLEGE
# 6: ASSOCIATE_DEGREE
# 7: IN_GRAD_SCHOOL
# 8: SOME_GRAD_SCHOOL
# 9: MASTER_DEGREE
# 10: PROFESSIONAL_DEGREE
# 11: DOCTORATE_DEGREE
# 12: UNSPECIFIED
# 13: SOME_HIGH_SCHOOL

param_df <- param_df %>%
  mutate(educ_status_simple = case_when(
    education_statuses == "1,4,13" ~ "High School",
    education_statuses == "2,5,6,7,8,9,10,11" ~ "More than high school",
    TRUE ~ education_statuses
  ))

# Simply Categories ------------------------------------------------------------
param_df$educ_status_simple[!is.na(param_df$educ_status_simple)] <-
  paste0("Eduction: ", param_df$educ_status_simple[!is.na(param_df$educ_status_simple)])

param_df$interest_name[!is.na(param_df$interest_name)] <-
  paste0("Interests: ", param_df$interest_name[!is.na(param_df$interest_name)])

param_df$behavior_name[!is.na(param_df$behavior_name)] <-
  paste0("Behavior: ", param_df$behavior_name[!is.na(param_df$behavior_name)])

param_df$param_name <- NA
param_df$param_name[!is.na(param_df$educ_status_simple)] <- param_df$educ_status_simple[!is.na(param_df$educ_status_simple)]
param_df$param_name[!is.na(param_df$interest_name)]      <- param_df$interest_name[!is.na(param_df$interest_name)]
param_df$param_name[!is.na(param_df$behavior_name)]      <- param_df$behavior_name[!is.na(param_df$behavior_name)]

write.csv(param_df, file.path(data_dir, SURVEY_NAME, "FinalData", "Individual Datasets", "facebook_marketing_parameters_clean.csv"), row.names = F)
