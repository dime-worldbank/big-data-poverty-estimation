# run_grid.py
#
# Description:
# Performs complete process of cleaning data and prepping all necessary data 
# and running the grid search.

# https://towardsdatascience.com/extract-features-visualize-filters-and-feature-maps-in-vgg16-and-vgg19-cnn-models-d2da6333edd0


import os, math, pickle, datetime, json
import numpy as np
import pandas as pd
import geopandas as gpd
import json
from rasterio.plot import show
import pickle

from geopandas import GeoDataFrame
from shapely.geometry import Point

from sklearn.decomposition import PCA
from sklearn.model_selection import train_test_split
from sklearn.preprocessing import StandardScaler
from sklearn.svm import LinearSVC
from sklearn.tree import DecisionTreeClassifier
from sklearn.ensemble import (BaggingClassifier, AdaBoostClassifier, 
                              GradientBoostingClassifier, RandomForestClassifier)
from sklearn.neighbors import KNeighborsClassifier
from sklearn.naive_bayes import GaussianNB
from sklearn.metrics import (accuracy_score, precision_score, 
                             recall_score, classification_report)

from keras.models import load_model
from keras.models import Sequential, Model

from tensorflow.keras.applications.vgg16 import preprocess_input

import warnings
import random
import tensorflow as tf
warnings.filterwarnings('ignore')

## User Defined
import config as cf
import feature_extraction as fe

def save_to_file(obj, path):
    '''
    Saves passed obj as a pickle to given filepath.
    '''
    with open(path, 'wb') as f:
        pickle.dump(obj=obj,
                    file=f,
                    protocol=pickle.HIGHEST_PROTOCOL)
    return None


def perform_pca(df, n):
    '''
    Performs PCA with n compponents on all columns in df.
    '''
    pca = PCA(n_components=n)
    pca.fit(df)
    features_pca = pca.transform(df)
    column_names = ['pc_%01d' %i for i in range(0,n)]
    df_features_pca = pd.DataFrame(data=features_pca, columns=column_names)
    return df_features_pca


def normalize(x_train, x_test):
    '''
    Normalize data.
    '''
    x_scaler = StandardScaler().fit(x_train)
    for df in (x_train, x_test):
        x_scaler.transform(df)

def extract_features_to_pd(param_name, bands_type):

    # 1. Load Data
    if bands_type == "RGB":
        DTL = np.load(os.path.join(cf.DROPBOX_DIRECTORY, 'Data', 'BISP' , 'FinalData', 'Individual Datasets', 'bisp_dtl_bands' + 'RGB' + '.npy'))
        bisp_df = pd.read_pickle(os.path.join(cf.DROPBOX_DIRECTORY, 'Data', 'BISP' , 'FinalData', 'Individual Datasets', 'bisp_dtl_uids_bands' + 'RGB' + '.pkl'))

    # 2. Extract features
    layer_name = 'fc1'

    model = load_model(os.path.join(cf.CNN_DIRECTORY, param_name, 'script_CNN.h5'))
    #model = load_model(cf.CNN_FILENAME)

    DTL_p = preprocess_input(DTL) # Preprocess image data

    #DTL_p = DTL_p[1:5,:,:,:] # for testing

    # Generate feature extractor using trained CNN
    feature_extractor = Model(inputs=model.inputs,
                              outputs=model.get_layer(name=layer_name).output,)

    features = feature_extractor.predict(DTL_p)

    # 3. Create and format pandas DataFrame
    df = pd.DataFrame(features).add_prefix('cnn_feat_')
    df['uid'] = bisp_df.uid
               
    # 4. Export           
    df.to_pickle(os.path.join(cf.DROPBOX_DIRECTORY, 'Data', 'BISP' , 'FinalData', 'Individual Datasets', 'bisp_cnn_features_all_' + param_name + '.pkl'))
    df.to_csv(os.path.join(cf.DROPBOX_DIRECTORY, 'Data', 'BISP' , 'FinalData', 'Individual Datasets', 'bisp_cnn_features_all_' + param_name + '.csv'))

if __name__ == '__main__':
    extract_features_to_pd("Nbands3_nNtlBins3_minNTLbinCount16861", "RGB")
