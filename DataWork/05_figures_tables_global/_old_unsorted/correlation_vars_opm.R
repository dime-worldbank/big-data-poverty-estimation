# Correlation Vars: OPM

# Load Data --------------------------------------------------------------------
df <- readRDS(file.path(data_dir, "OPM", "FinalData", "Merged Datasets", "survey_alldata_clean.Rds"))

df_cor <- df %>%
  dplyr::select_at(vars(country_name, 
                        pscores,
                        contains("fb_"),
                        contains("osm_"),
                        contains("worldpop_"),
                        contains("l8_"),
                        contains("elevslope_"),
                        contains("globalmod_"),
                        contains("pollution_"),
                        contains("gc_"),
                        contains("viirs_"))) %>%
  pivot_longer(cols = -c(country_name, pscores)) %>%
  group_by(name) %>%
  dplyr::summarise(cor = as.numeric(cor.test(pscores, value)$estimate)) %>%
  mutate(var_category = name %>% str_replace_all("_.*", "")) %>%
  mutate(r2 = cor^2) %>%
  mutate(var_category = case_when(
    # name %>% str_detect("osm_distmeters_poi_") ~ "OSM: Dist POI",
    # name %>% str_detect("osm_n_poi") ~ "OSM: N POI",
    # name %>% str_detect("osm_dist_") ~ "OSM: Dist Road",
    # name %>% str_detect("osm_N_segments_") ~ "OSM: N Road Segments",
    # name %>% str_detect("osm_length_") ~ "OSM: Road Length",
    # name %>% str_detect("osm_N_segments_") ~ "OSM: N Road Segments",
    var_category == "osm" ~ "OSM",
    var_category == "gc" ~ "Globcover",
    var_category == "viirs" ~ "NTL: VIIRS",
    TRUE ~ var_category
  ))






