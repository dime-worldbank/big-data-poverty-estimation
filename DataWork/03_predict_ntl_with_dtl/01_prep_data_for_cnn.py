# 00_prep_data_for_cnn.py

# Prepares data for CNN. Outputs numpy arrays and 
# parameter dictionary for CNN.

### Libraries ###
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

import logging, os 

### User Defined Libraries ###
import config as cf
import feature_extraction as fe

### Set Seeds ###
seed_value = 42
# 1. Set the `PYTHONHASHSEED` environment variable at a fixed value
os.environ['PYTHONHASHSEED'] = str(seed_value)
# 2. Set the `python` built-in pseudo-random generator at a fixed value
import random
random.seed(seed_value)
# 3. Set the `numpy` pseudo-random generator at a fixed value
np.random.seed(seed_value)

### Parameters / Paths ###
FINAL_TARGET_NAME = 'ntl_bins'
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


def normalize(X):
    '''
    Normalizes features.
    '''
    return X.astype('float32') / 255.0

def prep_cnn_data():

    # PARAMETERS -------------------------------------------------------------

    # Daytime impage parameters
    image_height = 48 # VGG16 needs images to be rescale to 224x224
    image_width = 48
    bands = ['4', '3', '2'] # which bands to use? 4,3,2 are RGB

    N_bands = len(bands)

    # Number of bins for NTL
    n_ntl_bins = 5

    # Minimum observations to take from each NTL bin
    min_ntl_bin_count = 6600

    #### Save parameters for later use
    cnn_param_dict = {'image_height': image_height, 
                    'image_width': image_width,
                    'bands': bands,
                    'N_bands': N_bands,
                    'n_ntl_bins': n_ntl_bins,
                    'min_ntl_bin_count': min_ntl_bin_count}

    print(cnn_param_dict)

    with open(cf.CNN_PARAMS_FILENAME, 'w') as fp:
        json.dump(cnn_param_dict, fp)

    # Run --------------------------------------------------------------------

    # LOAD DATA
    viirs = pd.read_pickle(VIIRS_GDF_FILEPATH)
    viirs_gdf = gpd.GeoDataFrame(viirs, geometry='geometry')
    viirs_gdf = viirs_gdf[ ~ np.isnan(viirs_gdf['tile_id'])]

    # PREP NTL
    transform_target(viirs_gdf, 'median_rad_2014', n_ntl_bins)

    # Total pixels in each category
    print(viirs_gdf[FINAL_TARGET_NAME].value_counts())

    # CREATE SAMPLE
    min_bin_count = min(viirs_gdf[FINAL_TARGET_NAME].value_counts())
    gdf = sample_by_target(viirs_gdf, FINAL_TARGET_NAME, min_ntl_bin_count)

    # MATCH DTL TO NTL
    DTL, processed_gdf = fe.map_DTL_NTL(gdf, DTL_DIRECTORY, bands, image_height, image_width)
    NTL = processed_gdf[FINAL_TARGET_NAME].to_numpy()

    print("Saving NTL")
    np.save(os.path.join(cf.DROPBOX_DIRECTORY, 'Data', 'CNN - Processed Inputs', 'ntl.npy'), NTL)

    print("Saving DTL")
    np.save(os.path.join(cf.DROPBOX_DIRECTORY, 'Data', 'CNN - Processed Inputs' , 'dtl.npy'), DTL)

if __name__ == '__main__':
    prep_cnn_data()
