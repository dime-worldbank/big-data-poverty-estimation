# Figures of DMSPOLS and VIIRS Along Addis-Adama Expressway

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

#geo_coords <- as.data.frame(geo_coords)

# Load Data --------------------------------------------------------------------
ls_b2 <- raster(file.path(raw_data_file_path, "Landsat Whole Country", "2014", "landsat_2014_pak_b2.tif"))
ls_b3 <- raster(file.path(raw_data_file_path, "Landsat Whole Country", "2014", "landsat_2014_pak_b3.tif"))
ls_b4 <- raster(file.path(raw_data_file_path, "Landsat Whole Country", "2014", "landsat_2014_pak_b4.tif"))

if(F){
  ls_b2[] %>% max(na.rm=T)
  ls_b3[] %>% max(na.rm=T)
  ls_b4[] %>% max(na.rm=T)
}

ls_b123 <- brick(ls_b2, ls_b3, ls_b4)

png(file.path(project_file_path, "Results", "Figures", "pak_landsat_naturalcolor_2014.png"),height=800, width=800)
plotRGB(ls_b123, r=3, g=2, b=1, scale=1.418, stretch="lin")
plot(geo_coords,add=T,col="firebrick1",pch=16,size=.02)
dev.off()
