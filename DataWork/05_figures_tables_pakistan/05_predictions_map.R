# Map of Predictions

# Load data --------------------------------------------------------------------
pakpoints_df <- readRDS(file.path(data_dir, "PAK_POINTS", 
                                  "FinalData", "Merged Datasets", "survey_alldata_clean_predictions.Rds"))

pak_adm_sp <- readRDS(file.path(gadm_dir, "RawData", "gadm36_PAK_0_sp.rds"))
pak_adm_sp_s <- gSimplify(pak_adm_sp, tol = 0.05)
pak_adm_sp_s$id <- 1

pakpoints_sp <- pakpoints_df
coordinates(pakpoints_sp) <- ~longitude+latitude
crs(pakpoints_sp) <- CRS("+init=epsg:4326")

#pakpoints_sp <- spTransform(pakpoints_sp, CRS("+init=epsg:24313"))
#pak_adm_sp_s <- spTransform(pak_adm_sp_s, CRS("+init=epsg:24313"))
#pakpoints_df <- as.data.frame(pakpoints_sp)

# Figure: Pakistan -------------------------------------------------------------
min_value <- pakpoints_df$predict_wealth_index_score %>% min(na.rm = T)
max_value <- pakpoints_df$predict_wealth_index_score %>% max(na.rm = T)

p_country <- ggplot() +
  geom_polygon(data = pak_adm_sp,
               aes(x = long, y = lat, group = group),
               fill = "black") +
  geom_tile(data = pakpoints_df,
            aes(x = longitude,
                y = latitude,
                fill = predict_wealth_index_score,
                width = 2.45/111.12,
                height = 2.45/111.12)) +
  coord_quickmap() +
  theme_void() +
  theme(legend.position = c(0.2, 0.78)) +
  scale_fill_viridis(labels = c("Least\nWealthly", "\nMost\nWealthly"),
                     breaks = c(min_value, max_value)) +
  labs(fill = "Predicted\nWealth\nScore") 

# Figure: Karachi --------------------------------------------------------------
make_city_figure <- function(city_name){
  
  buff <- 15/111.12
  
  if(city_name == "Karachi"){
    city_sp <- as(raster::extent(66.647193-buff, 67.500191+buff, 24.731122-buff, 25.112577+buff+10/111.12), "SpatialPolygons")
  } else if(city_name == "Lahore"){
    city_sp <- as(raster::extent(74.094614-buff, 74.566002+buff, 31.213211-buff, 31.728413+buff), "SpatialPolygons")
  } else if(city_name == "Faisalabad"){
    city_sp <- as(raster::extent(72.900377-buff, 73.274058+buff, 31.275037-buff, 31.580412+buff), "SpatialPolygons") 
  } else if(city_name == "Islamabad-Rawalpindi"){
    city_sp <- as(raster::extent(72.757109-buff, 73.394805+buff, 33.426272-buff, 33.806593+buff), "SpatialPolygons")
  }
  
  crs(city_sp) <- CRS("+init=epsg:4326")
  city_sp$id <- 1
  #city_sp <- spTransform(city_sp, CRS("+init=epsg:24313"))
  
  city_sp <- gIntersection(pak_adm_sp, city_sp)
  city_sp$id <- 1
  
  points_OVER_city <- over_chunks(pakpoints_sp, city_sp, fn_type = "none", 10000)
  
  city_points_sp <- pakpoints_sp[!is.na(points_OVER_city$id),]
  city_points_df <- as.data.frame(city_points_sp)
  
  ggplot() +
    geom_polygon(data = city_sp,
                 aes(x = long, y = lat, group = group),
                 fill = "black") +
    geom_tile(data = city_points_df,
              aes(x = longitude,
                  y = latitude,
                  fill = predict_wealth_index_score,
                  width = 2.45/111.12,
                  height = 2.45/111.12)) +
    theme_void() +
    theme(plot.title = element_text(face = "bold",
                                    hjust = 0.5),
          legend.position = "bottom") +
    scale_fill_viridis(labels = c("Least\nWealthly", "\nMost\nWealthly"),
                       breaks = c(min_value, max_value)) +
    labs(fill = "Predicted\nWealth\nScore",
         title = city_name) +
    coord_quickmap() 
}

p_city_1 <- make_city_figure("Karachi")
p_city_2 <- make_city_figure("Lahore")
p_city_3 <- make_city_figure("Faisalabad")
p_city_4 <- make_city_figure("Islamabad-Rawalpindi")

# Arrange and export -----------------------------------------------------------
p_city <- ggarrange(p_city_1,
                    p_city_2,
                    p_city_3,
                    p_city_4,
                    common.legend = TRUE,
                    legend = "bottom")

ggsave(p_country, 
       filename = file.path(figures_pak_dir, "pred_map_country.png"),
       height = 5.8,
       width = 6)

ggsave(p_city, 
       filename = file.path(figures_pak_dir, "pred_map_cities.png"),
       height = 7,
       width = 7)





