
library(randomForest)
library(caret)
library(xgboost)
library(adabag)

Sys.setenv("KMP_DUPLICATE_LIB_OK" = T) # temporary xgboost error fix
set.seed(42)

#### Load Data
bisp_df <- readRDS(file.path(final_data_file_path, "BISP", "Merged Datasets", "bisp_socioeconomic_satellite_panel_full_satPovNAsRemoved.Rds"))
bisp_df <- bisp_df[bisp_df$year %in% 2014,]

# Prep Variables ---------------------------------------------------------------
med <- median(bisp_df$pscores)
#bisp_df$pscores_bin <- (bisp_df$pscores <= 16.17) %>% as.numeric() 
bisp_df$pscores_bin <- bisp_df$pscores
bisp_df$estimate_dau_all_bin <- (bisp_df$estimate_dau_all > 0) %>% as.numeric() 

# Train/Test Sample ------------------------------------------------------------
trainIndex <- createDataPartition(bisp_df$pscores_bin, p = .8, 
                                  list = FALSE, 
                                  times = 1)



# Features ---------------------------------------------------------------------

#### All variables from dataset, including other survey variables
variables <- names(bisp_df)

#### Extract variables by category
landsat_features  <- variables[grepl("^b", variables) & grepl("_buff_", variables)]
viirs_features    <- variables[grepl("^viirs", variables)]
facebook_features <- variables[grepl("^estimate_", variables)]
osm_features      <- variables[grepl("^dist_osm_", variables)]

#### Group variables
all_features <- c(landsat_variables,
                   viirs_variables,
                   facebook_variables,
                   osm_variables)

#### Main features
# Grab what I think is relevant
main_features <- c('viirs_spatialmean_monthlymean_buff_2km',
                   'viirs_spatialmean_monthlysd_buff_2km',
                   'b12_buff_1km_mean',
                   'b13_buff_1km_mean',
                   'b14_buff_1km_mean',
                   'b15_buff_1km_mean',
                   'b16_buff_1km_mean',
                   'b17_buff_1km_mean',
                   'b23_buff_1km_mean',
                   'b24_buff_1km_mean',
                   'b25_buff_1km_mean',
                   'b26_buff_1km_mean',
                   'b27_buff_1km_mean',
                   'b34_buff_1km_mean',
                   'b35_buff_1km_mean',
                   'b36_buff_1km_mean',
                   'b37_buff_1km_mean',
                   'b45_buff_1km_mean',
                   'b46_buff_1km_mean',
                   'b47_buff_1km_mean',
                   'b56_buff_1km_mean',
                   'b57_buff_1km_mean',
                   'b67_buff_1km_mean',
                   'dist_osm_fclass_tertiary_meters',
                   'dist_osm_fclass_secondary_meters',
                   'dist_osm_fclass_residential_meters',
                   'dist_osm_fclass_trunk_meters',
                   'dist_osm_fclass_primary_meters',
                   'dist_osm_fclass_unclassified_meters',
                   'dist_osm_fclass_service_meters',
                   'dist_osm_fclass_motorway_meters',
                   'dist_osm_fclass_living_street_meters',
                   'estimate_dau_all',
                   'estimate_dau_male',
                   'estimate_dau_female')

# Prep Datasets ----------------------------------------------------------------
features <- all_features

train <- bisp_df[trainIndex,]
test  <- bisp_df[-trainIndex,]

train_features <- train[,features] %>% as.matrix()
test_features  <- test[,features] %>% as.matrix()

dtrain <- xgb.DMatrix(data = train_features, label=train$pscores_bin)
dtest  <- xgb.DMatrix(data = test_features,  label=test$pscores_bin)

watchlist <- list(train = dtrain, test = dtest)

# Adaboost ---------------------------------------------------------------------

# binary:logistic
bst <- xgb.train(data=dtrain, 
                 booster = "gbtree",
                 max.depth=10, eta=.2, nthread = 5, nrounds=100, num_parallel_tree = 5, watchlist=watchlist, objective = "reg:squarederror")

pred <- predict(bst, test_features)

plot(pred, test$pscores_bin)

cut_offs <- seq(from=0.1, to=.95, by=.05)
lapply(cut_offs, function(x) mean(test_features$pscores_bin == as.numeric(pred > x))) %>% unlist %>% max()









xgboost_result <- xgboost(data = dtrain, 
                     watchlist = watchlist,
                     max.depth = 2, eta = 1, nthread = 2, nrounds = 2, objective = "binary:logistic")

pred <- predict(xgboost_result, test_features_mat)

mean(test_features$pscores_bin == as.numeric(pred > 0.36))






model = boosting(pscores_bin ~ ., data=train_features, boos=T, mfinal=10)
pred = predict(model, test_features)


predicted_test <- c(pred$prob[,1] > 0.9) %>% as.numeric() %>% as.factor()

test_features$pscores_bin[pred$prob[,1] > 0.8] %>% table()

table(test_features$pscores_bin == predicted_test)


pred$confusion

str(train$pscores_bin)



data(agaricus.train, package='xgboost')
data(agaricus.test, package='xgboost')
train <- agaricus.train
test <- agaricus.test
