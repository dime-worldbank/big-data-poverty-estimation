# run_grid.py
#
# Description:
# Performs complete process of cleaning data and prepping all necessary data 
# and running the grid search.

# https://towardsdatascience.com/extract-features-visualize-filters-and-feature-maps-in-vgg16-and-vgg19-cnn-models-d2da6333edd0
# https://towardsdatascience.com/activation-maps-for-deep-learning-models-in-a-few-lines-of-code-ed9ced1e8d21
# https://github.com/philipperemy/keract

import os, math, pickle, datetime, json
import numpy as np
import pandas as pd
import geopandas as gpd
import json
from rasterio.plot import show
import pickle

import numpy as np
import tensorflow as tf
from tensorflow.keras import Input, Model
from tensorflow.keras.layers import Dense, concatenate
from keract import get_activations
from keras.models import load_model

import tensorflow
import tensorflow as tf
from keract import display_activations

## User Defined
import config as cf
import feature_extraction as fe


def extract_features_to_pd():

    # 1. Load Data and CNN Model
    DTL = np.load(os.path.join(cf.DROPBOX_DIRECTORY, 'Data', 'BISP' , 'FinalData', 'Individual Datasets', 'bisp_dtl.npy'))
    bisp_df = pd.read_pickle(os.path.join(cf.DROPBOX_DIRECTORY, 'Data', 'BISP' , 'FinalData', 'Individual Datasets', 'bisp_dtl_uids.pkl'))
    model = load_model(cf.CNN_FILENAME)

    # 2. Extract features
    layer_name = 'fc1'

    #DTL.shape
    #DTL[0].shape

    i = 0

    DTL_i = DTL[(i):(i+1)]

    l1 = DTL_i[0,:,:,0]
    show(l1)

    activations = get_activations(model, DTL[0:1])

    import keract
    keract.display_activations(activations, cmap=None, save=False, directory='.', data_format='channels_last', fig_size=(24, 24), reshape_1d_layers=False)

    keract.display_heatmaps(activations, DTL[0:1], save=False)



    a.shape


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
    df.to_pickle(os.path.join(cf.DROPBOX_DIRECTORY, 'Data', 'BISP' , 'FinalData', 'Individual Datasets', 'bisp_cnn_features_all.pkl'))
    df.to_csv(os.path.join(cf.DROPBOX_DIRECTORY, 'Data', 'BISP' , 'FinalData', 'Individual Datasets', 'bisp_cnn_features_all.csv'))

if __name__ == '__main__':
    extract_features_to_pd()
