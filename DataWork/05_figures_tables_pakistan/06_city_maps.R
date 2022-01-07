# Maps of City Data

# TO ADD:
# 1. Nighttime Lights
# 2. OSM variable(s)
# 3. Map of cities (ggmap)

# Load data --------------------------------------------------------------------
df <- readRDS(file.path(data_dir, SURVEY_NAME, "FinalData", "Merged Datasets", "survey_alldata_clean.Rds"))

## Facebook Clean
table(df$fb_estimate_mau_upper_bound_1 >= 150000)
df$fb_estimate_mau_upper_bound_1[df$fb_estimate_mau_upper_bound_1 >= 150000] <- 150000
#df$fb_estimate_mau_upper_bound_1[df$fb_estimate_mau_upper_bound_1 %in% 1000] <- NA

fb_param_df <- readRDS(file.path(data_dir, "Facebook Marketing", "FinalData", "facebook_marketing_parameters_clean.Rds"))
fb_param_df <- fb_param_df %>%
  dplyr::select(param_id, param_name_simple)

# Basemap figures --------------------------------------------------------------
if(F){
  get_map <- function(city_name, zoom){
    city <- df[df$city_name %in% city_name,]
    
    city_bbox <- c(left = min(city$longitude), 
                   bottom = min(city$latitude), 
                   right = max(city$longitude), 
                   top = max(city$latitude))
    
    city_smap  <- get_stamenmap(city_bbox, zoom = zoom)
    return(city_smap)
  }
  
  lahore_map <- get_map("Lahore", 11)
  karachi_map <- get_map("Karachi", 11)
  Faisalabad_map <- get_map("Faisalabad", 11)
  islamabad_map <- get_map("Islamabad-Rawalpindi", 11)
  
  ggmap(lahore_map) + theme_void()
  ggmap(karachi_map) + theme_void()
  ggmap(Faisalabad_map) + theme_void()
  ggmap(islamabad_map) + theme_void()
}

# Figures ----------------------------------------------------------------------
theme_fig <- theme(plot.title = element_text(face = "bold"),
                   strip.text = element_text(face = "bold")) 

n_fb <- df %>%
  ggplot() +
  geom_tile(aes(x = longitude,
                y = latitude,
                fill = fb_estimate_mau_upper_bound_1,
                width = 1.2/111.12,
                height = 1.2/111.12),
            alpha = 1) +
  scale_fill_viridis() +
  labs(fill = NULL,
       title = "N Monthly Active Facebook Users") +
  theme_void() +
  theme_fig +
  facet_wrap(~city_name, 
             scales = "free",
             nrow = 1) 

n_fb_prop_3 <- df %>%
  dplyr::mutate(fb_prop_estimate_mau_upper_bound_3 = case_when(
    fb_prop_estimate_mau_upper_bound_3 == 0 ~ NA_real_,
    TRUE ~ fb_prop_estimate_mau_upper_bound_3
  )) %>%
  ggplot() +
  geom_tile(aes(x = longitude,
                y = latitude,
                fill = fb_prop_estimate_mau_upper_bound_3,
                width = 1.2/111.12,
                height = 1.2/111.12),
            alpha = 1) +
  scale_fill_viridis() +
  labs(fill = NULL,
       title = "Proportion of Facebook Users: More than High School Education") +
  theme_void() +
  theme_fig +
  facet_wrap(~city_name, 
             scales = "free",
             nrow = 1) 

n_fb_prop_30 <- df %>%
  dplyr::mutate(fb_prop_estimate_mau_upper_bound_30 = case_when(
    fb_prop_estimate_mau_upper_bound_30 == 0 ~ NA_real_,
    TRUE ~ fb_prop_estimate_mau_upper_bound_30
  )) %>%
  ggplot() +
  geom_tile(aes(x = longitude,
                y = latitude,
                fill = fb_prop_estimate_mau_upper_bound_30,
                width = 1.2/111.12,
                height = 1.2/111.12),
            alpha = 1) +
  scale_fill_viridis() +
  labs(fill = NULL,
       title = "Proportion of Facebook Users: Interested in Luxury Goods") +
  theme_void() +
  theme_fig +
  facet_wrap(~city_name, 
             scales = "free",
             nrow = 1) 

n_fb_prop_7 <- df %>%
  dplyr::mutate(fb_prop_estimate_mau_upper_bound_7 = case_when(
    fb_prop_estimate_mau_upper_bound_7 == 0 ~ NA_real_,
    TRUE ~ fb_prop_estimate_mau_upper_bound_7
  )) %>%
  ggplot() +
  geom_tile(aes(x = longitude,
                y = latitude,
                fill = fb_prop_estimate_mau_upper_bound_7,
                width = 1.2/111.12,
                height = 1.2/111.12),
            alpha = 1) +
  scale_fill_viridis() +
  labs(fill = NULL,
       title = "Proportion of Facebook Users: Wifi") +
  theme_void() +
  theme_fig +
  facet_wrap(~city_name, 
             scales = "free",
             nrow = 1) 

p_gc_urban <- df %>%
  ggplot() +
  geom_tile(aes(x = longitude,
                y = latitude,
                fill = gc_190,
                width = 1.2/111.12,
                height = 1.2/111.12),
            alpha = 1) +
  scale_fill_viridis() +
  labs(fill = NULL,
       title = "Proporlation Land Urban [GlobCover]") +
  theme_void() +  
  theme_fig +
  facet_wrap(~city_name, 
             scales = "free",
             nrow = 1) 

p <- ggarrange(n_fb, 
               n_fb_prop_3, 
               n_fb_prop_30, 
               n_fb_prop_7, 
               p_gc_urban,
               ncol = 1)

ggsave(p, filename = file.path(figures_pak_dir, "city_variables.png"),
       height = 12, width = 10)

# OLD ==========================================================================
if(F){
  for(i in 2:35){
    
    df$var <- df[[paste0("fb_prop_estimate_mau_upper_bound_", i)]]
    
    p <- df %>%
      dplyr::mutate(var = case_when(
        var == 0 ~ NA_real_,
        TRUE ~ var
      )) %>%
      ggplot() +
      geom_tile(aes(x = longitude,
                    y = latitude,
                    fill = var,
                    width = 1.2/111.12,
                    height = 1.2/111.12),
                alpha = 1) +
      scale_fill_viridis() +
      labs(fill = NULL,
           title = i) +
      theme_void() +
      theme_fig +
      facet_wrap(~city_name, 
                 scales = "free",
                 nrow = 1) 
    
    ggsave(p, filename = file.path("~/Desktop", "temp", paste0(i, ".png")))
    
  }
}
