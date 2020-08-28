# CNN

# Load Data --------------------------------------------------------------------
viirs_sf <- readRDS(file.path(project_file_path, 
                              "Data", "VIIRS", "FinalData", 
                              "viirs_annual_polygon.Rds"))

DTL_PATH <- file.path(project_file_path, "Data", "Landsat", "RawData", "2014")

# Prep VIIRS -------------------------------------------------------------------
viirs_sf <- viirs_sf %>%
  mutate(median_rad_2014_log = log(median_rad_2014 + 1)) %>%
  mutate(ntl_bin = kmeans(median_rad_2014_log, centers = 5)$cluster) %>%
  
  # Order bins by value
  group_by(ntl_bin) %>%
  mutate(ntl_bin_avg = mean(median_rad_2014)) %>%
  ungroup() %>%
  
  # Ordered Bin
  mutate(ntl_bin = ntl_bin_avg %>% as.factor() %>% as.numeric()) 

# Prep Daytime Data ------------------------------------------------------------
r_target <- raster(nrow=32, ncol=32)


i <- 1
viirs_sf_i <- viirs_sf[i,]

b=1

r_target_i <- r_target
extent(r_target_i) <- extent(viirs_sf_i)

lapply(1:7, function(b){
  
  r <- raster(file.path(DTL_PATH,
                        paste0("l8_2014_tile", viirs_sf_i$tile_id, "_b", b, ".tif"))) %>%
    crop(extent(viirs_sf_i)) %>%
    resample(r_target_i, method = "bilinear") %>%
    as.matrix()
  
  return(r)
  
})







list.files(DTL_PATH)










ggplot(viirs_sf) +
  geom_histogram(aes(x = median_rad_2014_log), bins=0) + 
  facet_wrap(~ntl_bin2, scale = "free_y", ncol=1)

viirs_sf$ntl_bin <- kmeans(viirs_sf$median_rad_2014, centers = 5)$cluster

