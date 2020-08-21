# feature_extraction.py
#
# Description: Build and run CNN that will serve as feature extractor in poverty
#              estimation.

import os, datetime
import numpy as np
import pandas as pd
import geopandas as gpd

from sklearn.preprocessing import KBinsDiscretizer
from sklearn.model_selection import train_test_split, KFold
from sklearn.metrics import classification_report, confusion_matrix

from keras.utils import to_categorical
from keras.models import Sequential
from keras.layers import Conv2D, MaxPooling2D, Flatten, Dense
from keras.callbacks import EarlyStopping
from keras.callbacks import ModelCheckpoint
from keras.models import load_model

import logging, os 
logging.disable(logging.WARNING) 
os.environ["TF_CPP_MIN_LOG_LEVEL"] = "3"

import config as cf
import feature_extraction as fe

CNN_FILENAME = 'script_CNN.h5'
FINAL_TARGET_NAME = 'ntl_bins'

## MUST DEFINE ##
# CURRENT_DIRECTORY = "/Users/nguyenluong/wb_internship/Data/"
# VIIRS_GDF_FILEPATH = 'saved_objects/viirs_gdf.pkl'
# DTL_DIRECTORY = os.path.join('satellite_raw', 'Landsat', '2014')
CURRENT_DIRECTORY = cf.CURRENT_DIRECTORY
VIIRS_GDF_FILEPATH = cf.VIIRS_GDF_FILEPATH
DTL_DIRECTORY = cf.DTL_DIRECTORY


def transform_target(gdf, orig_target_name, n_bins):
    '''
    Creates log NTL variable and bins into 5 classes using k-means clutering.
    '''
    # Perform log(x+1) for defined domain
    transformed_target_name = f'log_{orig_target_name}'
    gdf[transformed_target_name] = np.log(gdf[orig_target_name] + 1)

    # Bin target
    target = gdf[transformed_target_name].to_numpy().reshape(-1,1)
    discretizer = KBinsDiscretizer(n_bins=n_bins, encode='ordinal', strategy='kmeans')
    gdf[FINAL_TARGET_NAME] = discretizer.fit_transform(target)


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


def evaluate_model(model, trainX, trainY, testX, testY):
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
    # Use early stopping to help with overfitting
    es = EarlyStopping(monitor='val_loss', mode='min', patience=5, verbose=False)
    # Save best model based on accuracy
    mc = ModelCheckpoint(CNN_FILENAME, monitor='val_accuracy', mode='max', 
                         verbose=False, save_best_only=True)
    
    # Fit model
    history = model.fit(trainX, trainY, 
                        epochs=10, batch_size=1000, 
                        validation_data=(testX, testY), callbacks=[es, mc], verbose=False)
    
    # Show accuracy
    loss, accuracy = model.evaluate(testX, testY, verbose=False)
    print(f'    Accuracy: {accuracy}')
        

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
    
    # Loop through folds
    count = 1
    for train_idx, test_idx in kfold.split(dataX):
        print(f'    --- Current K-fold: {count} ---')
        # Select subsets for training and testing
        trainX, trainY, testX, testY = dataX[train_idx], dataY[train_idx], \
                                       dataX[test_idx], dataY[test_idx]
        # Pass to evaluate_model function
        evaluate_model(model, trainX, trainY, testX, testY)
        count += 1


def display_eval_metrics(model, testX, testY):
    '''
    Displays evaluation metrics for a given trained model.
    '''
    # Get predictions
    predY = model.predict(testX)
    predY = np.argmax(predY, axis = 1)
    testY_bins = np.argmax(testY, axis = 1)

    # Generate classification report
    classes = ['Radiance Level %01d' %i for i in range(1,6)]
    print(classification_report(testY_bins, predY, target_names=classes, zero_division=0))


def main():

    # SET DIRECTORY
    os.chdir(CURRENT_DIRECTORY)
    print(f'{datetime.datetime.now()} 1. Directory set.')

    # LOAD DATA
    viirs = pd.read_pickle(VIIRS_GDF_FILEPATH)
    viirs_gdf = gpd.GeoDataFrame(viirs, geometry='geometry')
    viirs_gdf = viirs_gdf[ ~ np.isnan(viirs_gdf['tile_id'])]
    print(f'{datetime.datetime.now()} 2. Data loaded.')

    # PREP NTL
    n_bins = 5
    transform_target(viirs_gdf, 'median_rad_2014', n_bins)
    print(f'{datetime.datetime.now()} 3. NTL transformed.')

    # CREATE SAMPLE
    min_bin_count = min(viirs_gdf[FINAL_TARGET_NAME].value_counts(ascending=True))
    gdf = sample_by_target(viirs_gdf, FINAL_TARGET_NAME, min_bin_count)
    print(f'{datetime.datetime.now()} 4. Sample created.')

    # MATCH DTL TO NTL
    DTL, processed_gdf = fe.map_DTL_NTL(gdf, DTL_DIRECTORY)
    NTL = processed_gdf[FINAL_TARGET_NAME].to_numpy()
    print(f'{datetime.datetime.now()} 5. DTL and NTL matched.')

    # SPLIT DATA INTO TRAINING AND TESTING
    raw_trainX, raw_testX, raw_trainY, raw_testY = train_test_split(DTL, NTL, 
                                                                    test_size=0.2, 
                                                                    random_state=1)
    print(f'{datetime.datetime.now()} 6. Training and Testing sets defined.')

    # DEFINE IMAGE CHARACTERISTICS
    h, w, c = 25, 26, 7
    num_classes = 5
    
    # PREP TRAINING AND TESTING DATA
    trainX, trainY = prep_dataset(raw_trainX, raw_trainY, h, w, c)
    testX, testY = prep_dataset(raw_testX, raw_testY, h, w, c)
    
    # PREP PIXELS IN FEATURES
    trainX, testX = normalize(trainX), normalize(testX)
    print(f'{datetime.datetime.now()} 7. Data fully prepped.')

    # DEFINE AND EVALUTATE MODEL
    model = define_model(h, w, c, num_classes)
    evaluate_with_crossval(model, trainX, trainY, k=5)
    print(f'{datetime.datetime.now()} 8. Model defined and evaluated.')

    # DISPLAY IN-DEPTH EVALUTAION METRICS
    best_model = load_model(CNN_FILENAME)
    display_eval_metrics(best_model, testX, testY)


if __name__ == '__main__':
    main()

