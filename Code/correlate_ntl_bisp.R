# Correlate Nighttime Lights with BISP Data (Union Council Level)

# Setup ------------------------------------------------------------------------
if(Sys.info()[["user"]] == "robmarty") project_file_path <- "~/Dropbox/World Bank/IEs/Pakistan Poverty Estimation from Satellites"
if(Sys.info()[["user"]] == "WB521633") project_file_path <- "C:/Users/wb521633/Dropbox/World Bank/IEs/Pakistan Poverty Estimation from Satellites"

library(rgdal)
library(raster)
library(rgeos)
library(velox)

# Load Data --------------------------------------------------------------------
nser_uc <- readRDS(file.path(project_file_path, "Data", "FinalData", "UC Shapefile With NSER Data", "uc_nser.Rds"))

dmspols2010 <- raster(file.path(project_file_path, "Data", "RawData", "DMSPOLS", "pak_dmspols_2010.tif"))
viirs2012 <- raster(file.path(project_file_path, "Data", "RawData", "VIIRS", "VIIRS Annual","pak_viirs_median_2012.tif"))

# Extract NTL to Shapefiles ----------------------------------------------------
nser_uc$dmspols2010_mean <- velox(dmspols2010)$extract(nser_uc, fun=function(x,...) mean(x,na.rm=T))
nser_uc$dmspols2010_median <- velox(dmspols2010)$extract(nser_uc, fun=function(x,...) median(x,na.rm=T))

nser_uc$viirs2012_mean <- velox(viirs2012)$extract(nser_uc, fun=function(x,...) mean(x,na.rm=T))
nser_uc$viirs2012_median <- velox(viirs2012)$extract(nser_uc, fun=function(x,...) median(x,na.rm=T))

# Asset Index PCA --------------------------------------------------------------
nser_uc <- nser_uc@data
assets_for_index <- c("aircooler_p","freezer_p","washingmachine_p",
                      "airconditioner_p","heater_p","microwaveoven_p","cookingrange_p")

# Complete Cases of PCA Vars
complete_cases_assets <- complete.cases(nser_uc[,assets_for_index])
nser_uc[,assets_for_index] <- nser_uc[,assets_for_index][complete_cases_assets,]

# Compute PCA
pca <- princomp(nser_uc[,assets_for_index])
nser_uc$asset_index_pca <- pca$scores[,1]

# Correlations -----------------------------------------------------------------
cor.test(log(nser_uc$dmspols2010_median+1), nser_uc$asset_index_pca, method="pearson")
cor.test(log(nser_uc$viirs2012_median+0), nser_uc$asset_index_pca, method="pearson")

cor.test(log(nser_uc$dmspols2010_median+1), nser_uc$asset_item_index_mean, method="pearson")
cor.test(log(nser_uc$viirs2012_median+0), nser_uc$asset_item_index_mean, method="pearson")

cor.test(log(nser_uc$dmspols2010_median+1), nser_uc$pmt_score_mean, method="pearson")
cor.test(log(nser_uc$viirs2012_median+0), nser_uc$pmt_score_mean, method="pearson")

