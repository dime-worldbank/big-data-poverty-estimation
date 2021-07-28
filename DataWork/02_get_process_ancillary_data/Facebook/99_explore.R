# Merge Data Extracted from Facebook API

# Prep Parameter File ----------------------------------------------------------
params_df <- readRDS(file.path(data_dir, SURVEY_NAME, "FinalData", "Individual Datasets", "facebook_marketing_parameters.Rds"))

behaviors_df <- readRDS(file.path(project_file_path, "Data", "Facebook",  "FinalData", "interests_demographics_behaviors_ids","behaviors.Rds"))
interests_df <- readRDS(file.path(project_file_path, "Data", "Facebook",  "FinalData", "interests_demographics_behaviors_ids","interests.Rds"))

map_id_to_name <- function(i, params_df, id_var_name, id_names_df){
  params_df_i <- params_df[i,]
  
  id_clean <- params_df_i[[id_var_name]] %>% str_replace_all("\\}|\\{", " ")
  in_tf <- lapply(1:nrow(id_names_df), function(j) grepl(id_names_df$id[j], id_clean)) %>% unlist()
  
  out <- id_names_df$name[in_tf] %>% paste(collapse = ";")
  return(out)
}

params_df$behavior_names <- lapply(1:nrow(params_df), map_id_to_name, params_df, "behavior", behaviors_df) %>% unlist()
params_df$interest_names <- lapply(1:nrow(params_df), map_id_to_name, params_df, "interest", interests_df) %>% unlist()


# Load Data --------------------------------------------------------------------
SURVEY_NAME <- "DHS"

survey_pov <- readRDS(file.path(data_dir, SURVEY_NAME, "FinalData", "Individual Datasets", "survey_socioeconomic.Rds"))
fb_df <- readRDS(file.path(data_dir, SURVEY_NAME, "FinalData", "Individual Datasets", "facebook_marketing_dau_mau.Rds"))
fb_prop_df <- readRDS(file.path(data_dir, SURVEY_NAME, "FinalData", "Individual Datasets", "facebook_marketing_dau_mau_prop.Rds"))

fb_prop_df <- fb_prop_df %>% 
  rename_at(vars(-uid), ~ paste0(., '_prop'))

survey_pov <- survey_pov %>%
  left_join(fb_df, by = "uid") %>%
  left_join(fb_prop_df, by = "uid") %>%
  dplyr::filter(!is.na(estimate_mau_1))

survey_pov_long <- survey_pov %>%
  dplyr::filter(estimate_mau_1 > 1000) %>%
  dplyr::select(uid, wealth_index, wealth_index_score, urban_rural, contains("estimate_")) %>%
  pivot_longer(cols = -c(uid, wealth_index, wealth_index_score, urban_rural)) %>%
  dplyr::rename(fb_value = value,
                fb_variable = name)

cor_df <- survey_pov_long %>%
  dplyr::filter(!is.na(fb_value),
                fb_value != Inf) %>%
  group_by(fb_variable) %>%
  dplyr::summarise(cor_index = cor(wealth_index, fb_value),
                   cor_score = cor(wealth_index_score, fb_value)) %>%
  dplyr::filter(fb_variable %>% str_detect("mau"))


survey_pov %>%
  dplyr::filter(urban_rural %in% "U") %>%
  dplyr::filter(estimate_mau_19_prop < 0.5) %>%
  ggplot() +
  geom_point(aes(x = estimate_mau_19_prop,
                 y = wealth_index_score))

cor.test(survey_pov$wealth_index_score[survey_pov$estimate_mau_19_prop < 0.5],
         survey_pov$estimate_mau_19_prop[survey_pov$estimate_mau_19_prop < 0.5])


