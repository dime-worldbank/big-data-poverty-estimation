# feature_extraction.py
#
# Description:
# Build and run CNN that will serve as feature extractor in poverty estimation.

# https://www.analyticsvidhya.com/blog/2020/02/learn-image-classification-cnn-convolutional-neural-networks-3-datasets/
# https://github.com/jensleitloff/CNN-Sentinel
# https://github.com/jensleitloff/CNN-Sentinel/blob/master/slides/M3-2019_RieseLeitloff_SatelliteCV.pdf
# https://towardsdatascience.com/transfer-learning-for-image-classification-using-keras-c47ccf09c8c8
# https://colab.research.google.com/drive/18AN2AUM5sEsTMGUzFUL0FLSULtXF4Ps0#scrollTo=IadgsHtw48Of

import os, datetime
import numpy as np
import pandas as pd
import geopandas as gpd
import json
import rasterio
from rasterio.plot import show

from sklearn.preprocessing import KBinsDiscretizer
from sklearn.model_selection import train_test_split, KFold
from sklearn.metrics import classification_report, confusion_matrix

from keras.utils import to_categorical
from keras.models import Sequential, Model
from keras.layers import Conv2D, MaxPooling2D, Flatten, Dense, GlobalAveragePooling2D, Dropout
from keras.callbacks import EarlyStopping, ModelCheckpoint
from keras.models import load_model
from keras.applications.vgg16 import VGG16
from keras.applications.inception_v3 import preprocess_input

import logging, os 
logging.disable(logging.WARNING) 
os.environ["TF_CPP_MIN_LOG_LEVEL"] = "3"

import config as cf
import feature_extraction as fe

### FOR REPRODUCIBILITY ###
seed_value = 42
# 1. Set the `PYTHONHASHSEED` environment variable at a fixed value
os.environ['PYTHONHASHSEED'] = str(seed_value)
# 2. Set the `python` built-in pseudo-random generator at a fixed value
import random
random.seed(seed_value)
# 3. Set the `numpy` pseudo-random generator at a fixed value
np.random.seed(seed_value)
# 4. Set the `tensorflow` pseudo-random generator at a fixed value
import tensorflow as tf
#tf.random.set_seed(seed_value)
# 5. Configure a new global `tensorflow` session
#session_conf = tf.compat.v1.ConfigProto(intra_op_parallelism_threads=1, inter_op_parallelism_threads=1)
#sess = tf.compat.v1.Session(graph=tf.compat.v1.get_default_graph(), config=session_conf)
#tf.compat.v1.keras.backend.set_session(sess)

CNN_FILENAME = os.path.join(cf.DROPBOX_DIRECTORY, 'Models', 'CNN', 'script_CNN.h5')
#CNN_FILENAME = 'script_CNN.h5'
FINAL_TARGET_NAME = 'ntl_bins'
#CURRENT_DIRECTORY = cf.CURRENT_DIRECTORY
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


#def prep_dataset(X, Y, height, width, channels):
#    '''
#    Preps a given dataset for CNN by reshaping features and one-hot encoding targets.
#    '''
#    # Reshape features from 5D to 4D
#    X = X.reshape((X.shape[0], height, width, channels))
#    # One-hot encode targets from 1D to 2D
#    Y = to_categorical(Y)
#    return X, Y


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

def define_model_imagenet(height, width, channels, num_classes):
    '''
    Defines and compiles CNN model.
    
    Inputs:
        height, width, channels, num_classes (int)
    Returns:
        model (keras.Model object)
    '''

    # https://medium.com/abraia/first-steps-with-transfer-learning-for-custom-image-classification-with-keras-b941601fcad5
    # https://towardsdatascience.com/cnn-transfer-learning-fine-tuning-9f3e7c5806b2

    #### Base model
    input_shape = (height, width, channels)
    base_model = VGG16(weights='imagenet', include_top=False, input_shape=input_shape)

    for layer in base_model.layers:
        layer.trainable = False

    #### Model Customization
    # We take the last layer of our the model and add it to our classifier
    last = base_model.layers[-1].output
    x = Flatten()(last)
    x = Dense(100, activation='relu', name='fc1')(x)
    x = Dropout(0.3)(x)
    x = Dense(num_classes, activation='softmax', name='predictions')(x)
    model = Model(base_model.input, x)
    # We compile the model
    model.compile(optimizer='rmsprop',
              loss='categorical_crossentropy',
              metrics=['accuracy'])

    #model.compile(optimizer=Adam(lr=0.001), loss='categorical_crossentropy', metrics=['accuracy'])

    #x = base_model.output
    #x = GlobalAveragePooling2D(name='avg_pool')(x)
    #x = Dropout(0.4)(x)
    #x = Dense(100, activation='relu', name='dense1')(x)
    #predictions = Dense(num_classes, activation='softmax')(x)
    #model = Model(inputs=base_model.input, outputs=predictions)

    #model.compile(optimizer='rmsprop',
    #          loss='categorical_crossentropy',
    #          metrics=['accuracy'])

    return model


def evaluate_model(model, trainX, trainY, testX, testY):
    '''
    Fits model, evaluates model, saves best model over epochs and cross-validations.
    
    Inputs:
        model (CNN model) keras.Model object
        trainX, trainY (numpy.ndarray) 4D array of DTL features and 2D array of targets for training
        testX, testY (numpy.ndarray) 4D array of DTL features and 2D array of targets for testing
        current_kfold (int) iteration in kfold cross-val, default=None for no cross-val
        display_metrics (bool) Default=False
    Returns:
        None
    # https://towardsdatascience.com/step-by-step-guide-to-using-pretrained-models-in-keras-c9097b647b29
    '''

    # Use early stopping to help with overfitting
    es = EarlyStopping(monitor='val_loss', mode='min', patience=5, verbose=False)

    # Save best model based on accuracy
    mc = ModelCheckpoint(CNN_FILENAME, monitor='val_loss', mode='min', 
                         verbose=True, save_best_only=True)

    # Fit model
    model.fit(trainX, trainY, 
            epochs=10, 
            batch_size=500, 
            validation_data=(testX, testY), 
            callbacks=[es, mc], 
            verbose=False)

    # Show accuracy
    loss, accuracy = model.evaluate(testX, testY, verbose=False)
    print(f'                              Accuracy: {accuracy}')

    #return model
        

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
        print(f'{datetime.datetime.now()}    --- Current K-fold: {count} ---')
        # Select subsets for training and testing
        trainX, trainY, testX, testY = dataX[train_idx], dataY[train_idx], \
                                       dataX[test_idx], dataY[test_idx]
        # Pass to evaluate_model function
        evaluate_model(model, trainX, trainY, testX, testY)
        count += 1


def display_eval_metrics(model, testX, testY, n_ntl_bins):
    '''
    Displays evaluation metrics for a given trained model.
    '''
    # Get predictions
    predY = model.predict(testX)
    predY = np.argmax(predY, axis = 1)
    testY_bins = np.argmax(testY, axis = 1)
    # Generate classification report
    classes = ['Radiance Level %01d' %i for i in range(1,n_ntl_bins+1)]
    print(classification_report(testY_bins, predY, target_names=classes))

def buid_and_run_cnn():

    with open(cf.CNN_PARAMS_FILENAME, 'r') as fp:
        cnn_param_dict = json.load(fp)

    N_bands = cnn_param_dict['N_bands']
    n_ntl_bins = cnn_param_dict['n_ntl_bins']
    image_height = cnn_param_dict['image_height']
    image_width = cnn_param_dict['image_width']
    bands = cnn_param_dict['bands']
    min_ntl_bin_count = cnn_param_dict['bands']

    NTL = np.load(os.path.join(cf.DROPBOX_DIRECTORY, 'Data', 'CNN - Processed Inputs', 'ntl.npy'))
    DTL = np.load(os.path.join(cf.DROPBOX_DIRECTORY, 'Data', 'CNN - Processed Inputs', 'dtl.npy'))

    # SPLIT DATA INTO TRAINING AND TESTING
    trainX, testX, raw_trainY, raw_testY = train_test_split(DTL, NTL, 
                                                            test_size=0.2)

    # PREP TRAINING AND TESTING DATA
    trainY = to_categorical(raw_trainY)
    testY = to_categorical(raw_testY)

    #print(f'{datetime.datetime.now()} 4. Prepping Training and Testing Sets.')
    #trainX, trainY = prep_dataset(raw_trainX, raw_trainY, image_height, image_width, N_bands)
    #testX, testY = prep_dataset(raw_testX, raw_testY, image_height, image_width, N_bands)
    
    print(np.unique(NTL, return_counts=True))

    print(np.unique(raw_trainY, return_counts=True))
    print(np.unique(raw_testY, return_counts=True))

    print(np.unique(trainY, return_counts=True))
    print(np.unique(testY, return_counts=True))

    # PREP PIXELS IN FEATURES
    trainX, testX = normalize(trainX), normalize(testX)

    # DEFINE AND EVALUTATE MODEL
    print(f'{datetime.datetime.now()} 5. Defining and Evaluating CNN.')

    #model = define_model(image_height, image_width, N_bands, n_ntl_bins)
    model = define_model_imagenet(image_height, image_width, N_bands, n_ntl_bins)

    #evaluate_with_crossval(model, trainX, trainY, k=5)
    evaluate_model(model, trainX, trainY, testX, testY)

    # DISPLAY IN-DEPTH EVALUTAION METRICS
    best_model = load_model(CNN_FILENAME)
    display_eval_metrics(model, testX, testY, n_ntl_bins)
    print(f'{datetime.datetime.now()} 6. END: CNN Saved.') 

if __name__ == '__main__':
    buid_and_run_cnn()

