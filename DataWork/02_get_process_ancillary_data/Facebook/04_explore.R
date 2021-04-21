# Merge Data Extracted from Facebook API

# Load Data --------------------------------------------------------------------
opm_pov <- readRDS(file.path(data_dir, "OPM", "FinalData", "Individual Datasets", "bisp_socioeconomic.Rds"))
fb_prop_df <- readRDS(file.path(data_dir, "OPM", "FinalData", "Individual Datasets", "facebook_marketing_dau_mau_prop.Rds"))

opm_pov$uid <- opm_pov$uid %>% as.numeric()

opm_pov <- opm_pov %>%
  left_join(fb_prop_df, by = "uid") %>%
  dplyr::filter(!is.na(estimate_dau_20210414040732_2)) %>%
  dplyr::filter(year == 2014) %>%
  group_by(cluster_id) %>%
  summarise_if(is.numeric, mean) 

# opm_pov <- opm_pov %>%
#   dplyr::select(cluster_id, contains("mau"), pscores) %>%
#   pivot_longer(cols = -cluster_id) %>%
#   dplyr::filter(value =! Inf)

opm_pov$pscores %>% hist()
opm_pov$estimate_mau_20210417054957_11 %>% hist()

ggplot() +
  geom_point(data = opm_pov,
             aes(x = pscores,
                 y = estimate_dau_20210417140047_12))

opm_pov %>%
  names() %>%
  str_subset("dau")

lm(pscores ~ estimate_dau_20210414121920_3, data = opm_pov %>%
     dplyr::filter(estimate_dau_20210414121920_3 != Inf)) %>%
  summary()







