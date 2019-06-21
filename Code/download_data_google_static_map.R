# Extract Data from Google Static Maps
# https://blogs.bing.com/maps/2006/02/25/map-control-zoom-levels-gt-resolution

# Setup ------------------------------------------------------------------------
if(Sys.info()[["user"]] == "WB521633") project_file_path <- "C:/Users/wb521633/Dropbox/World Bank/IEs/Pakistan Poverty Estimation from Satellites"
if(Sys.info()[["user"]] == "robmarty") project_file_path <- "~/Dropbox/World Bank/IEs/Pakistan Poverty Estimation from Satellites"

library(RgoogleMaps)
library(raster)
library(dplyr)

# Create Points for Map --------------------------------------------------------
pakistan <- getData('GADM', country='PAK', level=1)
pakistan <- pakistan[pakistan$NAME_1 %in% "Punjab",]
pakistan_grid <- makegrid(pakistan, cellsize=.0014) # NOTE: need to optimize cell size so minimizes overlap between grids
pakistan_sp <- SpatialPoints(pakistan_grid, proj4string = CRS(proj4string(pakistan)))
pakistan_sp <- pakistan_sp[pakistan,]
pakistan_sp$id <- 1:length(pakistan_sp)

# Download Static Maps ---------------------------------------------------------
static_maps_file_directory <- file.path(project_file_path, "Data", "RawData", "Google Static Map API Data", "Zoom19")   

for(i in 1:nrow(pakistan_sp)){

  lon <- as.numeric(pakistan_sp[i,]@coords[1,1])
  lat <- as.numeric(pakistan_sp[i,]@coords[1,2])

  tmp <- GetMap(center = c(lat = lat, lon = lon),
              zoom=19,
              maptype = "satellite",
              API_console_key="", # API KEY HERE
              destfile=file.path(static_maps_file_directory,
                                 paste0("pak19_", gsub("\\.","x",paste0(abs(lat),"_",abs(lon))),".png")))
  
  print(i)
}

