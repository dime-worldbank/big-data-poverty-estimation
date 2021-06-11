# Make Grid

# https://stackoverflow.com/questions/47171710/create-a-grid-inside-a-shapefile

set.seed(42)
size = 200/111.12

# Need for intersection
ind0 <- readRDS(file.path(project_file_path, "Data", "GADM", "RawData", "gadm36_IND_0_sp.rds"))

pak <- readRDS(file.path(project_file_path, "Data", "GADM", "RawData", "gadm36_PAK_0_sp.rds"))
ind <- readRDS(file.path(project_file_path, "Data", "GADM", "RawData", "gadm36_IND_1_sp.rds"))
afg <- readRDS(file.path(project_file_path, "Data", "GADM", "RawData", "gadm36_AFG_0_sp.rds"))
bgd <- readRDS(file.path(project_file_path, "Data", "GADM", "RawData", "gadm36_BGD_0_sp.rds"))

ind <- ind[!(ind$NAME_1 %in% c("Andaman and Nicobar",
                             "Lakshadweep")),]
ind <- ind[,c("GID_0", "NAME_0")]

region <- rbind(pak, ind, afg)
region$id <- 1
region <- raster::aggregate(region, by = "id")
region$uid <- 1

region <- gSimplify(region, tol = 0.1)
region$uid <- 1
region_buff <- gBuffer(region, width = size/1.5, byid=T)

grdpts <- makegrid(region_buff, cellsize = size)
spgrd <- SpatialPoints(grdpts, proj4string = CRS(proj4string(region)))
spgrdWithin <- SpatialPixels(spgrd[region_buff,])
grid <- as(spgrdWithin, "SpatialPolygons")

grid$id <- 1:length(grid)

grid$intersect_pak <- gIntersects(grid, pak, byid=T) %>% as.numeric()
grid$intersect_ind <- gIntersects(grid, ind0, byid=T) %>% as.numeric()
grid$intersect_afg <- gIntersects(grid, afg, byid=T) %>% as.numeric()
grid$intersect_bgd <- gIntersects(grid, bgd, byid=T) %>% as.numeric()

grid <- grid[grid$intersect_pak | 
               grid$intersect_ind |
               grid$intersect_afg | 
               grid$intersect_bgd,]

# Export -----------------------------------------------------------------------
saveRDS(grid, file.path(project_file_path, "Data", "Country Grid", "FinalData", "pak_region_grid_200km.Rds"))

writeOGR(grid,
         dsn = file.path(project_file_path, "Data", "Country Grid", "FinalData", "shp"),
         layer = "pak_region_grid_200km",
         driver = "ESRI Shapefile",
         overwrite_layer = T,
         delete_dsn = T)

grid_sf <- st_as_sf(grid)
st_write(grid_sf, file.path(project_file_path, "Data", "Country Grid", "FinalData", "pak_region_grid_200km.geojson"),
         delete_dsn = T)


