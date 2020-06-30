# Figures of DMSPOLS and VIIRS Along Addis-Adama Expressway

# Load Data --------------------------------------------------------------------
ls_ndvi <- raster(file.path(raw_data_file_path, "Landsat Whole Country", "NDVI 2014", "landsat_2014_pak_NDVI.tif"))

# VIIRS Plot -----------------------------------------------------------------
ls_ndvi.df <- as(ls_ndvi, "SpatialPixelsDataFrame")
ls_ndvi.df <- as.data.frame(ls_ndvi.df)
colnames(ls_ndvi.df) <- c("value", "x", "y") 

ls_ndvi.df$value %>% sqrt %>% hist
p <- ggplot() +
  geom_tile(data=ls_ndvi.df, aes(x=x,y=y,fill=value)) +
  labs(colour="") +
  coord_equal() +
  theme_void() +
  labs(title="") +
  #scale_fill_gradientn(colours = terrain.colors(10)) +
  scale_fill_gradient2(name="NDVI", 
                       low = "saddlebrown", mid="goldenrod2", high = "forestgreen",midpoint=-.1,
                       na.value = 'black',
                       trans="sqrt") +
  theme(legend.position = "none") 
ggsave(p, filename=file.path(file.path(project_file_path, "Results", "Figures", "pak_ndvi_2014_sqrt.png")), height=6, width=6)

