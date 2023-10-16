df <- readRDS(file.path(data_dir, SURVEY_NAME, "FinalData", "Merged Datasets", "survey_alldata.Rds"))

df %>%
  ggplot() +
  geom_point(aes(x = worldpop_2km,
                 y = log(fb_estimate_mau_upper_bound_1))) 

df %>%
  ggplot() +
  geom_point(aes(x = worldpop_2km,
                 y = l8_NDVI)) 

df %>%
  ggplot() +
  geom_point(aes(x = worldpop_2km,
                 y = viirs_avg_rad)) 

df %>%
  ggplot() +
  geom_point(aes(x = worldpop_2km,
                 y = osm_length_residential_5000m)) 

df %>%
  ggplot() +
  geom_point(aes(x = worldpop_2km,
                 y = viirs_avg_rad)) 

df %>%
  ggplot() +
  geom_point(aes(x = fb_rwi,
                 y = log(viirs_avg_rad+1))) 

df %>%
  ggplot() +
  geom_point(aes(x = fb_rwi,
                 y = osm_length_residential_5000m)) 
