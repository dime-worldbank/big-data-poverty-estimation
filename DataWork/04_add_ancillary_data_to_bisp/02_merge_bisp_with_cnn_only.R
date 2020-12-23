# CNN Models

library(glmnet)
set.seed(42)

# Load Data --------------------------------------------------------------------
bisp_df <- readRDS(file.path(project_file_path, "Data", "BISP", 
                             "FinalData", "Individual Datasets", "bisp_socioeconomic.Rds"))
cnn_df <- read.csv(file.path(bisp_indiv_files_dir, "bisp_cnn_features_all_Nbands3_nNtlBins3_minNTLbinCount16861.csv"))

bisp_df$uid <- bisp_df$uid %>% as.character() %>% as.numeric
cnn_df$uid <- cnn_df$uid %>% as.character() %>% as.numeric

cnn_df$year <- 2014

# Merge ------------------------------------------------------------------------
bisp_df <- merge(bisp_df, cnn_df, by = c("uid", "year"))
bisp_df <- bisp_df %>%
  filter(!is.na(pscores))

bisp_df$pscores <- as.numeric(bisp_df$pscores <= 16.17)

train <- sample(size = nrow(bisp_df), x = c(T, F), prob = c(.8, .2), replace = T)

x <- bisp_df[,grepl("cnn_", names(bisp_df))] %>% data.matrix()
y <- bisp_df$pscores
lambdas <- 10^seq(3, -2, by = -.1)

cv_fit <- cv.glmnet(x[train,], y[train], alpha = 0, lambda = lambdas)
opt_lambda <- cv_fit$lambda.min
fit <- cv_fit$glmnet.fit
y_predicted <- predict(fit, s = opt_lambda, newx = x[!train,])[,1]




lm(pscores ~ cnn_feat_0 + cnn_feat_1 + cnn_feat_2 + cnn_feat_3, data = bisp_df) %>% summary()



# Export -----------------------------------------------------------------------
saveRDS(bisp_satdata_df, file.path(final_data_file_path, "BISP", "Merged Datasets", "bisp_socioeconomic_satellite_panel_full.Rds"))
write.csv(bisp_satdata_df, file.path(final_data_file_path, "BISP", "Merged Datasets", "bisp_socioeconomic_satellite_panel_full.csv"), row.names=F)
