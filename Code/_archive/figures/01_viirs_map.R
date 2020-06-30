# Figures of DMSPOLS and VIIRS Along Addis-Adama Expressway

# Load Data --------------------------------------------------------------------
viirs <- raster(file.path(raw_data_file_path, "VIIRS", "VIIRS Annual", "pak_viirs_median_2012.tif"))

# VIIRS Plot -----------------------------------------------------------------
viirs.df <- as(viirs, "SpatialPixelsDataFrame")
viirs.df <- as.data.frame(viirs.df)
colnames(viirs.df) <- c("value", "x", "y") 

viirs.df$value_adj <- (viirs.df$value) %>% sqrt %>% sqrt
p <- ggplot() +
  geom_tile(data=viirs.df, aes(x=x,y=y,fill=value_adj)) +
  labs(colour="") +
  coord_equal() +
  theme_void() +
  labs(title="") +
  scale_fill_gradient2(name="Nighttime Lights", 
                       low = "black", mid="orange", high = "white",midpoint=1.9,
                       limits = c(0.6258,3.2388),
                       na.value = 'black') +
  theme(legend.position = "none") 
ggsave(p, filename=file.path(file.path(project_file_path, "Results", "Figures", "pak_viirs_2012.png")), height=6, width=6)

