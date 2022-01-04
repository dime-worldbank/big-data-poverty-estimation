# Make points in select Pakistan Cities

PAK_UTM <- "+init=epsg:24312"

set.seed(42)

# Make Grid --------------------------------------------------------------------
pak_adm <- readRDS(file.path(gadm_dir, "RawData", "gadm36_PAK_0_sp.rds"))

lahore_sp <- as(raster::extent(74.094614, 74.566002, 31.213211, 31.728413), "SpatialPolygons")
lahore_sp$name <- "Lahore"

karachi_sp <- as(raster::extent(66.647193, 67.500191, 24.731122, 25.112577), "SpatialPolygons")
karachi_sp$name <- "Karachi"

faisalabad_sp <- as(raster::extent(72.900377, 73.274058, 31.275037, 31.580412), "SpatialPolygons") 
faisalabad_sp$name <- "Faisalabad"

isl_raw_sp <- as(raster::extent(72.757109, 73.394805, 33.426272, 33.806593), "SpatialPolygons")
isl_raw_sp$name <- "Islamabad-Rawalpindi"

pak_cities <- rbind(lahore_sp,
                    karachi_sp,
                    faisalabad_sp,
                    isl_raw_sp)
crs(pak_cities) <- CRS("+init=epsg:4326")

## Make points
city_df <- map_df(unique(pak_cities$name), function(city_i){
  
  pak_cities <- pak_cities[pak_cities$name %in% city_i,]
  
  pak_cities <- spTransform(pak_cities, CRS(PAK_UTM))
  pak_cities_grid <- sp::makegrid(pak_cities, cellsize = 1000)

  pak_cities_grid$name <- city_i
  pak_cities_grid$uid <- paste0(city_i, "_", 1:nrow(pak_cities_grid)) %>% tolower()
  
  return(pak_cities_grid)
}) 

## Spatially define
coordinates(city_df) <- ~x1+x2
crs(city_df) <- CRS(PAK_UTM)
city_df <- spTransform(city_df, CRS("+init=epsg:4326"))

## Must be in Pakistan
# Some points cross border and go over wayer

city_OVER_pak <- over(city_df, pak_adm)

city_df <- city_df[city_OVER_pak$NAME_0 %in% "Pakistan",]

# Export -----------------------------------------------------------------------


head(city_df)




