# CNN Models

library(glmnet)
set.seed(42)

CNN_PCA <- T

# Load Data --------------------------------------------------------------------
## BISP Data
bisp_df <- readRDS(file.path(project_file_path, "Data", "BISP", 
                             "FinalData", "Individual Datasets", "bisp_socioeconomic.Rds"))

## Ancillary Data
cnn_df <- read.csv(file.path(bisp_indiv_files_dir, "bisp_cnn_features_all_Nbands3_nNtlBins3_minNTLbinCount16861_2014.csv"))
cnn_cont_df <- read.csv(file.path(bisp_indiv_files_dir, "bisp_cnn_features_all_Nbands3_nNtlBins3_minNTLbinCount16861_2014_cont.csv"))
viirs_df <- readRDS(file.path(bisp_indiv_files_dir, "bisp_viirs.Rds"))
landsat_df <- readRDS(file.path(bisp_indiv_files_dir, "bisp_landsat.Rds"))

# VIIRS/Landsat Prep -----------------------------------------------------------
## VIIRS
viirs_df <- viirs_df %>%
  dplyr::select(uid,
                viirs_buff1km_year2014_spatialMEAN_monthlyMEAN, 
                viirs_buff5km_year2014_spatialMEAN_monthlyMEAN)

## Landsat
landsat_df <- landsat_df[,grepl("uid|2014_buff_2km_mean", names(landsat_df))]
names(landsat_df) <- names(landsat_df) %>% str_replace_all("_2014_buff_2km_mean", "")

# Construct indices
bands <- paste0("b", 1:7)
bands_combn <- combn(bands, 2)

make_indicie <- function(var1, var2, df){
  (df[[var1]] - df[[var2]]) / (df[[var1]] + df[[var2]])
}

for(i in 1:ncol(bands_combn)){
  
  var_newname <- bands_combn[,i] %>% paste(collapse = "_")
  landsat_df[[var_newname]] <- make_indicie(bands_combn[,i][1],
                                            bands_combn[,i][2],
                                            landsat_df)
  
}

viirs_landsat_df <- merge(viirs_df, landsat_df, by = "uid")

# Prep CNN ---------------------------------------------------------------------
if(CNN_PCA){
  
  cnn_pca <- cnn_df %>%
    dplyr::select(-c(X, uid)) %>%
    prcomp()
  cnn_pca_df <- cnn_pca$x[,1:10] %>% as.data.frame()
  names(cnn_pca_df) <- paste0("cnn_", 1:ncol(cnn_pca_df))
  cnn_pca_df$uid <- cnn_df$uid
  
  cnn_cont_pca <- cnn_cont_df %>%
    dplyr::select(-c(X, uid)) %>%
    prcomp()
  cnn_cont_pca_df <- cnn_cont_pca$x[,1:10] %>% as.data.frame()
  names(cnn_cont_pca_df) <- paste0("cnn_", 1:ncol(cnn_cont_pca_df))
  cnn_cont_pca_df$uid <- cnn_cont_df$uid
  
  cnn_df      <- cnn_pca_df
  cnn_cont_df <- cnn_cont_pca_df
}

# Merge with BISP --------------------------------------------------------------
## BISP Prep/Merge
bisp_df <- bisp_df %>%
  dplyr::filter(year %in% 2014)

bisp_virrs_landsat_df <- merge(bisp_df, viirs_landsat_df, by = "uid")

## Subset
bisp_virrs_landsat_df <- bisp_virrs_landsat_df %>%
  filter(!is.na(b1),
         !is.na(pscores))

## BISP Transformations
bisp_virrs_landsat_df$pscores_poor <- as.numeric(bisp_virrs_landsat_df$pscores <= 16.17)
bisp_virrs_landsat_df$pscores_poor_med <- as.numeric(bisp_virrs_landsat_df$pscores < median(bisp_virrs_landsat_df$pscores))
bisp_virrs_landsat_df$asset_index_additive_bin <- as.numeric(bisp_virrs_landsat_df$asset_index_additive < median(bisp_virrs_landsat_df$asset_index_additive))
bisp_virrs_landsat_df$asset_index_pca1_bin <- as.numeric(bisp_virrs_landsat_df$asset_index_pca1 < median(bisp_virrs_landsat_df$asset_index_pca1))

## Merge CNN
bisp_cnn_df      <- merge(bisp_virrs_landsat_df, cnn_df,      by = "uid")
bisp_cnn_cont_df <- merge(bisp_virrs_landsat_df, cnn_cont_df, by = "uid")

# Export -----------------------------------------------------------------------
saveRDS(bisp_cnn_df, file.path(project_file_path, "Data", "BISP", "FinalData", "Merged Datasets", "cnn_merge.Rds"))
write.csv(bisp_cnn_df, file.path(project_file_path, "Data", "BISP", "FinalData", "Merged Datasets", "cnn_merge.csv"), row.names=F)

saveRDS(bisp_cnn_cont_df, file.path(project_file_path, "Data", "BISP", "FinalData", "Merged Datasets", "cnn_cont_merge.Rds"))
write.csv(bisp_cnn_cont_df, file.path(project_file_path, "Data", "BISP", "FinalData", "Merged Datasets", "cnn_cont_merge.csv"), row.names=F)



