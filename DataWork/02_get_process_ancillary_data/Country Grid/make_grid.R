# Make Grid

# https://stackoverflow.com/questions/47171710/create-a-grid-inside-a-shapefile

size = 200/111.12

pak <- readRDS(file.path(project_file_path, "Data", "GADM", "RawData", "gadm36_PAK_0_sp.rds"))
pak <- gSimplify(pak, tol = 0.1)
pak_buff <- gBuffer(pak, width = size/2, byid=T)

grdpts <- makegrid(pak_buff, cellsize = size)
spgrd <- SpatialPoints(grdpts, proj4string = CRS(proj4string(pak)))
spgrdWithin <- SpatialPixels(spgrd[pak_buff,])
grid <- as(spgrdWithin, "SpatialPolygons")

grid$id <- 1:length(grid)

plot(grid)
plot(pak,add=T)

# Export -----------------------------------------------------------------------

saveRDS(grid, file.path(project_file_path, "Data", "Pakistan Grid", "RawData", "pak_grid_200km.Rds"))

writeOGR(grid,
         dsn = file.path(project_file_path, "Data", "Pakistan Grid", "RawData", "shp"),
         layer = "pak_grid_200km",
         driver = "ESRI Shapefile")

grid_sf <- st_as_sf(grid)
write_sf(grid_sf, file.path(project_file_path, "Data", "Pakistan Grid", "RawData", "pak_grid_200km.geojson"))


