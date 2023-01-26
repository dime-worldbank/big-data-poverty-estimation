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
data <- read.csv(file.path(final_data_file_path, "Data with Predicted Income", "opm_data_with_predictions_testdatamodel_alldatapredict.csv"))

data <- merge(data, geo_coords, by="uid")

# Predicted vs Actual Map: Points ----------------------------------------------
pak_adm0_simp <- gSimplify(pak_adm0, tol=.035)
data$y_true_f <- data$y_true %>% as.character()
data$y_true_f[data$y_true_f == "0"] <- "Low"
data$y_true_f[data$y_true_f == "1"] <- "Medium"
data$y_true_f[data$y_true_f == "2"] <- "High"

data$y_predict_f <- data$y_predict_49 %>% as.character()
data$y_predict_f[data$y_predict_f == "0"] <- "Low"
data$y_predict_f[data$y_predict_f == "1"] <- "Medium"
data$y_predict_f[data$y_predict_f == "2"] <- "High"

data$y_true_f <- data$y_true_f %>% factor(levels=c("Low", "Medium", "High"))
data$y_predict_f <- data$y_predict_f %>% factor(levels=c("Low", "Medium", "High"))

# FULL MAP
pmap <- ggplot() +
  geom_polygon(data=pak_adm0_simp, aes(x=long, y=lat, group=group), fill="peachpuff3", color="gray10") +
  geom_point(data=data, aes(x=longitude, y=latitude, color=y_true_f)) +
  theme_void() +
  scale_colour_viridis(discrete=T) +
  labs(color = "Household\nIncome") +
  coord_quickmap()
ggsave(pmap, filename=file.path(file.path(project_file_path, "Results", "Figures", "poverty_3level_actual_predicted_map_true.png")), height=6, width=6)

pmap <- ggplot() +
  geom_polygon(data=pak_adm0_simp, aes(x=long, y=lat, group=group), fill="peachpuff3", color="gray10") +
  geom_point(data=data, aes(x=longitude, y=latitude, color=y_predict_f)) +
  theme_void() +
  scale_colour_viridis(discrete=T) +
  labs(color = "Household\nIncome\nLevel") +
  coord_quickmap()
ggsave(p_map, filename=file.path(file.path(project_file_path, "Results", "Figures", "poverty_3level_actual_predicted_map_points.png")), height=6, width=6)



# Judge Accuracy ---------------------------------------------------------------
accuracy_score <- function(values1, values2){
  return(mean(values1 == values2))
}

acc_df <- lapply(1:70, function(i){
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

