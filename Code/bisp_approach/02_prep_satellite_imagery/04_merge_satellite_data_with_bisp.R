# Merge Satellite Data with BISP Survey Data

# Load Data --------------------------------------------------------------------
landsat <- raster(file.path(rawdata_file_path, "Landsat", "bisp_households", "unstacked", "17", "B1_median.tif"))
plot(landsat)

landsat[]

coordinates(landsat)
