# PCA of MOSAIKS

# Load data --------------------------------------------------------------------
mosaik_df <- readRDS(file.path(data_dir, SURVEY_NAME, "FinalData", 
                               "Individual Datasets", "mosaik.Rds"))

# Cleanup ----------------------------------------------------------------------
#### Remove variables with no variation
sd_df <- mosaik_df %>%
  summarise_if(is.numeric, sd) %>%
  t() %>%
  as.data.frame() %>%
  dplyr::rename(sd = V1)

sd_df$variable <- row.names(sd_df)

sd_df <- sd_df %>%
  dplyr::filter(sd > 0)

mosaik_df <- mosaik_df[,names(mosaik_df) %in% c("uid", sd_df$variable)]

# Calculate PC -----------------------------------------------------------------

mosaik_noid_df <- mosaik_df %>%
  dplyr::select(-uid)

# Calculate the principal components
pca <- prcomp(mosaik_noid_df, scale = TRUE)

# Get the cumulative explained variance
cum_var <- cumsum(pca$sdev^2 / sum(pca$sdev^2))

# Find the number of principal components that explain 90% of the variation
n_components <- min(which(cum_var >= 0.999))

# Print the variables that correspond to the selected principal components
pca_vars_df <- pca$x[, 1:n_components] %>% as.data.frame()

names(pca_vars_df) <- paste0("mosaik_pca_", 1:ncol(pca_vars_df))

pca_vars_df$uid <- mosaik_df$uid

# Export -----------------------------------------------------------------------
saveRDS(pca_vars_df, file.path(data_dir, SURVEY_NAME, "FinalData", 
                               "Individual Datasets", "mosaik_pca.Rds"))



