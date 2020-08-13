# feature_extraction.py
#
# Description: Build and run CNN that will serve as feature extractor in poverty
#              estimation.
# Current author: Nguyen Luong 
# Updated: 8/13/20

import os
import numpy as np
import pandas as pd
import geopandas as gpd

from keras.utils import to_categorical
from sklearn.model_selection import train_test_split, KFold
from sklearn.metrics import classification_report, confusion_matrix

from keras.models import Sequential
from keras.layers import Convolution2D
from keras.layers import AveragePooling2D
from keras.layers import Dropout
from keras.layers import Flatten
from keras.layers import Dense
from keras.layers import Activation
from keras.callbacks import EarlyStopping
from keras.callbacks import ModelCheckpoint

import feature_extraction as fe


def transform_target(gdf, target_col_name, n_bins):
    '''
    Creates log NTL variable and bins into 5 classes using k-means clutering.
    '''
    # Perform log(x+1) for defined domain
    tranformed_col_name = f'log_{target_col_name}'
    gdf[transformed_col_name] = np.log(gdf[target_col_name] + 1)

    # Bin target
    target = gdf[transformed_col_name].to_numpy().reshape(-1,1)
    discretizer = KBinsDiscretizer(n_bins=n_bins, encode='ordinal', strategy='kmeans')
    gdf['target_binned'] = discretizer.fit_transform(target)


def sample_by_target(input_gdf, target_col_name, n):
    '''
    Create a sample dataframe containing n observations from each target bin.
    '''
    gdf = gpd.GeoDataFrame()

    for x in input_gdf[target_col_name].unique():
        bin_gdf = input_gdf[input_gdf[target_col_name] == x]
        sample_gdf = bin_gdf.sample(n=n, random_state=1)
        gdf = gdf.append(sample_gdf)

    return gdf


def prep_dataset(X, Y, height, width, channels):
    '''
    Preps a given dataset for CNN by reshaping features and one-hot encoding targets.
    '''
    # Reshape features from 5D to 4D
    X = X.reshape((X.shape[0], height, width, channels))
    # One-hot encode targets from 1D to 2D
    Y = to_categorical(Y)

    return X, Y


def normalize(X):
    '''
    Normalizes features.
    '''
    return X.astype('float32') / 255.0


def define_model(height, width, channels, num_classes):
    '''
    Defines and compiles CNN model.
    
    Inputs:
        height, width, channels, num_classes (int)
    Returns:
        model (keras.Model object)
    '''
    # Define layers
    model = Sequential()
    model.add(Conv2D(filters=64, 
                     kernel_size=(5, 5), 
                     activation='relu', 
                     input_shape=(height, width, channels),
                     name='conv1'))
    model.add(MaxPooling2D(pool_size=(2, 2), name='maxpool1'))
    model.add(Flatten(name='flatten1'))
    model.add(Dense(100, activation='relu', name='dense1'))
    model.add(Dense(num_classes, activation='softmax', name='dense2'))
    
    # Compile model
    model.compile(optimizer='rmsprop', loss='categorical_crossentropy', metrics=['accuracy'])
    
    return model


def evaluate_model(model, trainX, trainY, testX, testY, current_kfold=None, 
                   display_metrics=False):
    '''
    Fits and evaluates model.
    
    Inputs:
        model (CNN model) keras.Model object
        trainX, trainY (numpy.ndarray) 4D array of DTL features and 2D array of targets for training
        testX, testY (numpy.ndarray) 4D array of DTL features and 2D array of targets for testing
        current_kfold (int) iteration in kfold cross-val, default=None for no cross-val
        display_metrics (bool) Default=False
    Returns:
        None
    '''
    print('---Current K-fold: {}---'.format(current_kfold))
    
    # Use early stopping to help with overfitting
    es = EarlyStopping(monitor='val_loss', mode='min', patience=5, verbose=True)
    mc = ModelCheckpoint('resample_pretrain_CNN.h5', monitor='val_accuracy', mode='max', 
                         verbose=True, save_best_only=True)
    
    # Fit model
    history = model.fit(trainX, trainY, 
                        epochs=10, batch_size=1000, 
                        validation_data=(testX, testY), callbacks=[es, mc], verbose=True)
    
    # Evaluate model
    loss, accuracy = model.evaluate(testX, testY, verbose=True)
    print('Accuracy: {:.4f}'.format(accuracy))
    print()
    
    if display_metrics:
        # Get predictions
        predY = model.predict(testX)
        predY = np.argmax(predY, axis = 1)
        testY_bins = np.argmax(testY, axis = 1)
    
        # Generate classification report
        classes = ['Radiance Level 1', 'Radiance Level 2', 'Radiance Level 3', 
                   'Radiance Level 4', 'Radiance Level 5'] 
        print(classification_report(testY_bins, predY, target_names=classes))
        print()
        

def evaluate_with_crossval(model, dataX, dataY, k=2):
    '''
    Performs evaulation with K-fold cross validation.
    
    Inputs:
        model (keras.Model object)
        dataX, dataY (numpy.ndarray) 4D array of DTL features and 2D array of targets 
                                     for training
        k (int)
    Returns:
        None
    '''
    # Define k-fold cross-val
    kfold = KFold(k, shuffle=True, random_state=1)
    print('Entering cross validation.')
    
    # Loop through folds
    counter = 0
    for train_idx, test_idx in kfold.split(dataX):
        counter += 1
        # Select subsets for training and testing
        trainX, trainY, testX, testY = dataX[train_idx], dataY[train_idx], 
                                       dataX[test_idx], dataY[test_idx]
        # Pass to evaluate_model
        if counter == k:
            # If last k-fold, then display full evaluation metrics 
            evaluate_model(model, trainX, trainY, testX, testY, current_kfold=counter, display_metrics=True)
        else:
            evaluate_model(model, trainX, trainY, testX, testY, current_kfold=counter)


def main():

    # SET DIRECTORY
    os.chdir("/Users/nguyenluong/wb_internship/Data/satellite_raw")

    # LOAD DATA
    viirs_gdf = pd.read_pickle('../saved_objects/viirs_gdf.pkl')
    viirs_gdf = viirs_gdf[['tile_id', 'median_rad_2014', 'geometry']]
    viirs_gdf = viirs_gdf[ ~ np.isnan(viirs_gdf['tile_id'])]

    # PREP NTL
    n_bins = 5
    transform_target(viirs_gdf, 'median_rad_2014', n_bins)

    # CREATE SAMPLE
    n = 1000
    gdf = sample_by_target(viirs_gdf, 'target_binned', n)

    # MATCH DTL TO NTL
    DTL_directory = os.path.join('Landsat', '2014')
    DLT, processed_gdf = fe.map_DTL_NTL(gdf, DTL_directory)
    NTL = processed_gdf['target_binned'].to_numpy()

    # SPLIT DATA INTO TRAINING AND TESTING
    raw_trainX, raw_testX, raw_trainY, raw_testY = train_test_split(DTL, NTL, 
                                                                    test_size=0.2, 
                                                                    random_state=1)
    # DEFINE IMAGE CHARACTERISTICS
    h, w, c = 50, 50, 3
    num_classes = 5
    
    # PREP TRAINING AND TESTING DATA
    trainX, trainY = prep_dataset(raw_trainX, raw_trainY, h, w, c)
    testX, testY = prep_dataset(raw_testX, raw_testY, h, w, c)
    
    # PREP PIXELS IN FEATURES
    trainX, testX = normalize(trainX), normalize(testX)

    # DEFINE AND EVALUTATE MODEL
    model = define_model(h, w, c, num_classes)
    evaluate_with_crossval(model, trainX, trainY)


if __name__ == '__main__':
    main()

