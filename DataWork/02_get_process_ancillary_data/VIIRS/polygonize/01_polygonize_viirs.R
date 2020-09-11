# Polygonize Annual VIIRS

library(spex)

viirs_path = file.path(project_file_path, "Data", "VIIRS", "RawData", "VIIRS Annual")

viirs2012_p <- raster(file.path(viirs_path, "pak_viirs_median_2012.tif")) %>% spex::polygonize(na.rm = T)
viirs2013_p <- raster(file.path(viirs_path, "pak_viirs_median_2013.tif")) %>% spex::polygonize(na.rm = T)
viirs2014_p <- raster(file.path(viirs_path, "pak_viirs_median_2014.tif")) %>% spex::polygonize(na.rm = T)
viirs2015_p <- raster(file.path(viirs_path, "pak_viirs_median_2015.tif")) %>% spex::polygonize(na.rm = T)
viirs2016_p <- raster(file.path(viirs_path, "pak_viirs_median_2016.tif")) %>% spex::polygonize(na.rm = T)
viirs2017_p <- raster(file.path(viirs_path, "pak_viirs_median_2017.tif")) %>% spex::polygonize(na.rm = T)
viirs2018_p <- raster(file.path(viirs_path, "pak_viirs_median_2018.tif")) %>% spex::polygonize(na.rm = T)

virrs_p <- viirs2012_p %>%
  dplyr::rename(median_rad_2012 = pak_viirs_median_2012)
virrs_p$median_rad_2013 <- viirs2013_p$pak_viirs_median_2013
virrs_p$median_rad_2014 <- viirs2014_p$pak_viirs_median_2014
virrs_p$median_rad_2015 <- viirs2015_p$pak_viirs_median_2015
virrs_p$median_rad_2016 <- viirs2016_p$pak_viirs_median_2016
virrs_p$median_rad_2017 <- viirs2017_p$pak_viirs_median_2017
virrs_p$median_rad_2018 <- viirs2018_p$pak_viirs_median_2018

virrs_p$id <- 1:nrow(virrs_p)

# Export -----------------------------------------------------------------------
saveRDS(virrs_p, file.path(project_file_path, "Data", "VIIRS", "FinalData", "viirs_annual_polygon.Rds"))
st_write(virrs_p, file.path(project_file_path, "Data", "VIIRS", "FinalData", "viirs_annual_polygon.geojson"))






