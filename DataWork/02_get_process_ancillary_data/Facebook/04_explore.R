# Merge Data Extracted from Facebook API

# Load Data --------------------------------------------------------------------
SURVEY_NAME <- "DHS"

survey_pov <- readRDS(file.path(data_dir, SURVEY_NAME, "FinalData", "Individual Datasets", "survey_socioeconomic.Rds"))
fb_df <- readRDS(file.path(data_dir, SURVEY_NAME, "FinalData", "Individual Datasets", "facebook_marketing_dau_mau.Rds"))
fb_prop_df <- readRDS(file.path(data_dir, SURVEY_NAME, "FinalData", "Individual Datasets", "facebook_marketing_dau_mau_prop.Rds"))
params_df <- readRDS(file.path(data_dir, SURVEY_NAME, "FinalData", "Individual Datasets", "facebook_marketing_parameters.Rds"))

fb_prop_df <- fb_prop_df %>% 
  rename_at(vars(-uid), ~ paste0(., '_prop'))

survey_pov <- survey_pov %>%
  left_join(fb_df, by = "uid") %>%
  left_join(fb_prop_df, by = "uid") %>%
  dplyr::filter(!is.na(estimate_mau_1))

survey_pov_long <- survey_pov %>%
  dplyr::select(uid, wealth_index, wealth_index_score, urban_rural, contains("estimate_")) %>%
  pivot_longer(cols = -c(uid, wealth_index, wealth_index_score, urban_rural)) %>%
  dplyr::rename(fb_value = value,
                fb_variable = name)

cor_df <- survey_pov_long %>%
  group_by(fb_variable) %>%
  dplyr::summarise(cor_index = cor(wealth_index, fb_value),
                   cor_score = cor(wealth_index_score, fb_value)) %>%
  dplyr::filter(fb_variable %>% str_detect("mau"))

survey_pov %>%
  ggplot() +
  geom_point(aes(x = estimate_mau_18_prop,
                 y = wealth_index_score))






# opm_pov <- opm_pov %>%
#   dplyr::select(cluster_id, contains("mau"), pscores) %>%
#   pivot_longer(cols = -cluster_id) %>%
#   dplyr::filter(value =! Inf)

opm_pov$pscores %>% hist()
opm_pov$estimate_mau_20210417054957_11 %>% hist()

ggplot() +
  geom_point(data = opm_pov,
             aes(x = pscores,
                 y = estimate_mau_20210415044235_5))

opm_pov %>%
  names() %>%
  str_subset("mau")

lm(pscores ~ estimate_dau_20210414121920_3, data = opm_pov %>%
     dplyr::filter(estimate_dau_20210414121920_3 != Inf)) %>%
  summary()







