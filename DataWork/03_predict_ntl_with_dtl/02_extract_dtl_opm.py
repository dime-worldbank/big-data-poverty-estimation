# 02_extract_dtl_bisp.py
#
# Description:
# Extracts numpy array of DTL around BISP coordinates and saves a 
# pandas dataframe (pickled) of the UIDs corresponding to each 
# numpy array. Processes data so that features can be extracted
# from the numpy array using the trained CNN model.

import os, math, pickle, datetime, json
import numpy as np
import pandas as pd
import geopandas as gpd
import json
from rasterio.plot import show

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

import warnings
import random
import tensorflow as tf
warnings.filterwarnings('ignore')

## User Defined
import config as cf
import feature_extraction as fe

def pd_to_gdp(df, lat_name = 'latitude', lon_name = 'longitude'):
    '''
    Converts a pandas dataframe with lat and long variables into
    geopandas point data

    Input:  df - pandas dataframe
            lat_name - name of latitude variable in df
            lon_name - name of longitude variable in df
    Output: geopandas dataframe
    '''

    geometry = [Point(xy) for xy in zip(df[lon_name], df[lat_name])]
    df = df.drop([lon_name, lat_name], axis=1)
    gdf = GeoDataFrame(df, crs="EPSG:4326", geometry=geometry)

    return gdf

def extract_dtl_bisp(bands_type, year):

    # 0. Load Bands
    if bands_type == "RGB":
        bands = ['4', '3', '2']

    # 1. Load Grid for DTL Tiles
    dtl_tiles = gpd.read_file(os.path.join(cf.DROPBOX_DIRECTORY, 'Data', 'Country Grid', 'RawData', 'pak_grid_200km.geojson'))
    dtl_tiles.rename(columns = {'id': 'tile_id'}, inplace=True)

    # 2. Prep BISP data
    # Load, convert to geopandas, extract dtl tile
    #bisp_df = pd.read_csv(os.path.join('/Users/robmarty', 'Desktop', 'GPS_uid_crosswalk2.csv'))
    bisp_df = pd.read_csv(os.path.join(cf.SECURE_DATA_DIRECTORY, 'Data', 'BISP', 'FinalData - PII', 'GPS_uid_crosswalk.csv'), engine = 'python')
    bisp_gdf = pd_to_gdp(bisp_df)
    bisp_gdf = gpd.sjoin(bisp_gdf, dtl_tiles, how="inner", op='intersects').reset_index(drop=True)
    bisp_gdf['geometry'] = bisp_gdf.buffer(distance = 0.75/111.12).envelope
    
    # 3. Extract DTL to BISP Coordinates

    # Load CNN parameters 
    # Use this for paramers; just effects img_height and img_weidth
    param_name = "Nbands3_nNtlBins3_minNTLbinCount16861"
    PARAM_PATH_JSON = os.path.join(cf.CNN_DIRECTORY, param_name, 'CNN_parameters.json')

    with open(PARAM_PATH_JSON, 'r') as fp:
        cnn_param_dict = json.load(fp)

    # Extract
    DTL, bisp_gdf_processed = fe.map_DTL_NTL(input_gdf = bisp_gdf, 
                                        directory = os.path.join(cf.DTL_DIRECTORY, str(year)), 
                                        bands = bands, 
                                        img_height = cnn_param_dict['image_height'], 
                                        img_width = cnn_param_dict['image_width'],
                                        year = year)

    bisp_gdf_processed = bisp_gdf_processed[['uid']]

    # 4. Export
    np.save(os.path.join(        cf.DROPBOX_DIRECTORY, 'Data', 'BISP' , 'FinalData', 'Individual Datasets', 'bisp_dtl_bands' + bands_type + "_" + str(year) + '.npy'), DTL)
    bisp_gdf_processed.to_pickle(os.path.join(cf.DROPBOX_DIRECTORY, 'Data', 'BISP' , 'FinalData', 'Individual Datasets', 'bisp_dtl_uids_bands' + bands_type + "_" + str(year) + '.pkl'))

if __name__ == '__main__':
    extract_dtl_bisp("RGB", 2014)



