# to test feasibility of approach...

# Prep Geocoords ---------------------------------------------------------------
pak_adm0 <- getData('GADM', country='PAK', level=0)
pak_adm <- getData('GADM', country='PAK', level=3)

geo_coords <- read.csv(file.path("~/Desktop/GPS_uid_crosswalk.csv"))
geo_coords <- geo_coords[!is.na(geo_coords$GPSN),]

# Coords
get_lat_lon <- function(number){
  
  deg = floor(number / 100)
  min = floor(number - (100 * deg))
  sec = 100 * (number - (100 * deg) - min)
  degree = deg + (min / 60) + (sec / 3600)
  
  return(degree)
}

geo_coords$latitude <- get_lat_lon(geo_coords$GPSN)
geo_coords$longitude <- get_lat_lon(geo_coords$GPSE)

geo_coords <- geo_coords[geo_coords$latitude < 90,]
geo_coords <- geo_coords[geo_coords$longitude < 90,]

coordinates(geo_coords) <- ~longitude+latitude
crs(geo_coords) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

geo_coords_OVER_adm <- over(geo_coords, pak_adm)
geo_coords$NAME_0 <- geo_coords_OVER_adm$NAME_0
geo_coords$NAME_1 <- geo_coords_OVER_adm$NAME_1
geo_coords$NAME_2 <- geo_coords_OVER_adm$NAME_2
geo_coords$NAME_3 <- geo_coords_OVER_adm$NAME_3

geo_coords <- geo_coords[!is.na(geo_coords$NAME_0),]

geo_coords <- as.data.frame(geo_coords)

# Load Data --------------------------------------------------------------------
#data <- read.csv(file.path(final_data_file_path, "Data with Predicted Income", "testdata_with_predictions.csv"))
data <- read.csv(file.path(final_data_file_path, "Data with Predicted Income", "pov_opm_data_with_predictions_testdatamodel_alldatapredict.csv"))

data <- merge(data, geo_coords, by="uid")

# Predicted vs Actual Map: Points ----------------------------------------------
pak_adm0_simp <- gSimplify(pak_adm0, tol=.035)
data$y_true_f <- data$y_true %>% as.character()
data$y_true_f[data$y_true_f == "True"] <- "Yes"
data$y_true_f[data$y_true_f == "False"] <- "No"

data$y_predict_f <- data$y_predict_21 %>% as.character()
data$y_predict_f[data$y_predict_f == "True"] <- "Yes"
data$y_predict_f[data$y_predict_f == "False"] <- "No"

data$y_true_f <- data$y_true_f %>% factor(levels=c("Yes", "No"))
data$y_predict_f <- data$y_predict_f %>% factor(levels=c("Yes", "No"))

# FULL MAP
pmap <- ggplot() +
  geom_polygon(data=pak_adm0_simp, aes(x=long, y=lat, group=group), fill="gray75", color="gray10") +
  geom_point(data=data, aes(x=longitude, y=latitude, color=y_true_f)) +
  theme_void() +
  scale_colour_manual(values=c("darkorange1", "dodgerblue3")) +
  labs(color = "BISP\nBeneficiary",title="From Survey Data") +
  theme(plot.title = element_text(hjust=.5, face="bold")) +
  coord_quickmap()
ggsave(pmap, filename=file.path(file.path(project_file_path, "Outputs", "Results", "Figures", "poverty_3level_actual_predicted_map_points_true.png")), height=5, width=5)

pmap <- ggplot() +
  geom_polygon(data=pak_adm0_simp, aes(x=long, y=lat, group=group), fill="gray75", color="gray10") +
  geom_point(data=data, aes(x=longitude, y=latitude, color=y_predict_f)) +
  theme_void() +
  scale_colour_manual(values=c("darkorange1", "dodgerblue3")) +
  labs(color = "BISP\nBeneficiary", title="Predicted from Satellite Imagery") +
  theme(plot.title = element_text(hjust=.5, face="bold")) +
  coord_quickmap()
ggsave(pmap, filename=file.path(file.path(project_file_path, "Outputs", "Results", "Figures", "poverty_3level_actual_predicted_map_points_predicted.png")), height=5, width=5)

# Restricted Map
center <- data.frame(id = 1,
                     lat = 34.009802,
                     lon = 72.310956)
coordinates(center) <- ~lon+lat
center_buff <- gBuffer(center, width=7/111.12)
#center_buff@bbox

basemap <- get_stamenmap(bbox = c(
  left = center_buff@bbox[1,1] - 3.5/111.12,
  right = center_buff@bbox[1,2] + 3.5/111.12,
  bottom = center_buff@bbox[2,1],
  top = center_buff@bbox[2,2]),
  zoom = 13,
  maptype = "terrain")

pmap <- ggmap(basemap) +
  geom_point(data=data, aes(x=longitude, y=latitude, fill=y_true_f), size=2, pch=21, color="black") +
  theme_void() +
  scale_fill_manual(values=c("darkorange1", "dodgerblue3")) +
  labs(fill = "BISP\nBeneficiary") +
  coord_quickmap()
pmap
ggsave(pmap, filename=file.path(file.path(project_file_path, "Results", "Figures", "poverty_3level_actual_predicted_map_points_true_RESTRICT.png")), height=6, width=6)

pmap <- ggmap(basemap) +
  geom_point(data=data, aes(x=longitude, y=latitude, fill=y_predict_f), size=2, pch=21, color="black") +
  theme_void() +
  scale_fill_manual(values=c("darkorange1", "dodgerblue3")) +
  labs(fill = "BISP\nBeneficiary") +
  coord_quickmap()
ggsave(pmap, filename=file.path(file.path(project_file_path, "Results", "Figures", "poverty_3level_actual_predicted_map_points_predicted_RESTRICT.png")), height=6, width=6)




get_stamenmap(bbox = c(left = -95.80204, bottom = 29.38048, right =
                         -94.92313, top = 30.14344), zoom = 10, maptype = c("terrain",
                                                                            "terrain-background", "terrain-labels", "terrain-lines", "toner",
                                                                            "toner-2010", "toner-2011", "toner-background", "toner-hybrid",
                                                                            "toner-labels", "toner-lines", "toner-lite", "watercolor"),
              crop = TRUE, messaging = FALSE, urlonly = FALSE,
              color = c("color", "bw"), force = FALSE, where = tempdir(), ...)


leaflet() %>%
  addTiles() %>%
  addCircles(data=data, lng=~longitude, lat=~latitude)




data_long <- bind_rows(
  data[,c("uid", "y_predict_f")] %>% mutate(type = "Predictions") %>% dplyr::rename(value = y_predict_f),
  data[,c("uid", "y_true_f")] %>% mutate(type = "OPM Data") %>% dplyr::rename(value = y_true_f)
)




# Bar Map
p <- ggplot() +
  geom_bar(data=data_long, aes(x=value,
                               group=type,
                               fill=type),
           position = "dodge", color="black") +
  scale_fill_manual(values=c("navajowhite3", "lightskyblue"),
                    guide = guide_legend(reverse = TRUE)) +
  labs(x="BISP\nBeneficiary",
       title = "Number of Households\nClassified as BISP Beneficiaries",
       fill = "",
       y="") +
  theme_minimal() +
  theme(axis.title.y = element_text(angle=0, vjust=.5, face="bold", size=13),
        plot.title = element_text(face="bold", hjust=.5, size=13),
        axis.text = element_text(size=13),
        legend.text = element_text(size=13)) +
  coord_flip() 
p
ggsave(p, filename=file.path(file.path(project_file_path, "Results", "Figures", "poverty_3level_actual_predicted_map_points_fig.png")), height=3, width=6)


#

# Judge Accuracy ---------------------------------------------------------------
accuracy_score <- function(values1, values2){
  return(mean(values1 == values2))
}

acc_df <- lapply(1:48, function(i){
  print(i)
  data$y_true <- data$y_true %>% as.character
  data[[paste0("y_predict_", i)]] <- data[[paste0("y_predict_", i)]] %>% as.character
  
  df_out <- data.frame(i = i,
                       acc = accuracy_score(data$y_true, data[[paste0("y_predict_", i)]]))
  
  return(df_out)
}) %>% bind_rows

# ADM Level Dataframe ----------------------------------------------------------
data$N <- 1
data_N <- summaryBy(N ~ NAME_3, data=data, FUN=sum, na.rm=T, keep.names=T)
data_adm <- summaryBy(y_predict_49 + y_predict_50 + y_true ~ NAME_3, data=data, FUN=mean, na.rm=T, keep.names=T)
data_adm <- merge(data_adm, data_N, by="NAME_3")
data_adm <- data_adm[data_adm$N >= 5,]

# Choose Prediction ------------------------------------------------------------
data_adm$y_predict <- data_adm$y_predict_49

# Scatterplot ------------------------------------------------------------------
p_scatter <- ggplot() + 
  geom_point(data=data_adm, aes(x=y_true, y=y_predict), color="dodgerblue3",size=1.5) +
  labs(x="Income Level from OPM Data",
       y="Income\nLevel:\nPredicted",
       title="Average Income Level Across Districts",
       subtitle="Actual vs Predicted Income") + 
  theme_minimal() +
  theme(axis.title.y = element_text(angle=0, vjust=0.5, face="bold.italic"),
        axis.title.x = element_text(face="bold.italic"),
        axis.text = element_text(size=13),
        plot.title = element_text(hjust=0.5, face="bold"),
        plot.subtitle = element_text(hjust=0.5, face="bold")) +
  scale_x_continuous(breaks = c(0, 1, 2),
                     limits = c(0, 2),
                     labels = c("Low", "Medium", "High")) +
  scale_y_continuous(breaks = c(0, 1, 2),
                     limits = c(0, 2),
                     labels = c("Low", "Medium", "High"))
ggsave(p_scatter, filename=file.path(file.path(project_file_path, "Results", "Figures", "poverty_3level_actual_predicted_scatter.png")), height=3.5, width=5.5)

# Map --------------------------------------------------------------------------
pak_adm <- merge(pak_adm, data_adm, by="NAME_3", all.x=T, all.y=F)
pak_adm_lim <- pak_adm[!is.na(pak_adm$y_true),]

pak_adm0_simp <- gSimplify(pak_adm0, tol=.035)
pak_adm0_simp$id <- 1

pak_adm_lim_simp <- gSimplify(pak_adm_lim, tol=.035)
pak_adm_lim_simp$id <- 1:nrow(pak_adm_lim)
pak_adm_lim_simp@data <- pak_adm_lim@data

pak_adm_lim$id <- row.names(pak_adm_lim)
pak_adm_lim_tidy <- broom::tidy(pak_adm_lim)
pak_adm_lim_tidy <- merge(pak_adm_lim_tidy, pak_adm_lim, by="id")

pak_adm_lim_tidy_true <- pak_adm_lim_tidy %>% 
  dplyr::mutate(value = y_true) %>%
  dplyr::mutate(type = "From OPM Data")
pak_adm_lim_tidy_predict <- pak_adm_lim_tidy %>% 
  dplyr::mutate(value = y_predict) %>%
  dplyr::mutate(type = "Predicted")

pak_adm_lim_tidy_stack <- bind_rows(pak_adm_lim_tidy_predict,
                                    pak_adm_lim_tidy_true)

p_map <-ggplot() +
  geom_polygon(data=pak_adm0_simp, aes(x=long, y=lat, group=group), fill="gray70") +
  geom_polygon(data=pak_adm_lim_tidy_stack, 
               aes(x=long, y=lat, group=group, fill=value)) +
  theme_void() +
  coord_quickmap() +
  scale_fill_viridis_c(breaks = c(0, 1, 2),
                       limits = c(0, 2),
                       labels = c("Low", "Medium", "High")) +
  labs(fill = "Average\nIncome\nLevel") +
  theme(strip.text = element_text(hjust=0.5, face="bold.italic", size=12),
        legend.position="bottom") +
  #scale_fill_distiller(palette = "Spectral", direction=-1) + 
  facet_wrap(~type)
ggsave(p_map, filename=file.path(file.path(project_file_path, "Results", "Figures", "poverty_3level_actual_predicted_map.png")), height=6, width=6)

