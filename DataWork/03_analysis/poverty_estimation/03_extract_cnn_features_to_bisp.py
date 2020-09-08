# run_grid.py
#
# Description:
# Performs complete process of cleaning data and prepping all necessary data 
# and running the grid search.

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



def extract_cnn_data():

    # 1. Load Data
    DTL = np.load(os.path.join(cf.DROPBOX_DIRECTORY, 'Data', 'BISP' , 'FinalData', 'Individual Datasets', 'bisp_dtl.npy'))
    bisp_df = pd.read_pickle(os.path.join(cf.DROPBOX_DIRECTORY, 'Data', 'BISP' , 'FinalData', 'Individual Datasets', 'bisp_dtl_uids.pkl'))

    # 2. Extract features
    # https://towardsdatascience.com/extract-features-visualize-filters-and-feature-maps-in-vgg16-and-vgg19-cnn-models-d2da6333edd0

    layer_name = 'dense1'
    model = load_model(cf.CNN_FILENAME)

    #df_features = fe.extract_features(model, DTL, layer_name)




    # Preprocess image data
    DTL_p = preprocess_input(DTL)



    # Generate feature extractor using trained CNN
    feature_extractor = Model(inputs=model.inputs,
                              outputs=model.get_layer(name=layer_name).output,)



    a = feature_extractor.predict(DTL_p)
                              

    # Extract features and convert from tensor to numpy array
    # features = feature_extractor(DTL_p).numpy()
    features = feature_extractor(DTL_p)

    # Create and format pandas DataFrame
    df = pd.DataFrame(features).add_prefix('feat_')




    # USE PCA TO SELECT EXTRACTED FEATURES
    print(f'{datetime.datetime.now()} 3. Performing PCA.')
    df_features_pca = perform_pca(df_features, 10)
    feature_dict['EXTRACTED_FEATURES'] = df_features_pca.columns.to_list()
    feature_dict['EXTRACT_OSM_FB_FEATURES'] = feature_dict['EXTRACTED_FEATURES'] \
                                        + feature_dict['OSM_FB_FEATURES']




    ############# DONE ############






    

    # 1. Load Grid for DTL Tiles
    dtl_tiles = gpd.read_file(os.path.join(cf.DROPBOX_DIRECTORY, 'Data', 'Pakistan Grid', 'RawData', 'pak_grid_200km.geojson'))
    dtl_tiles.rename(columns = {'id': 'tile_id'}, inplace=True)

    # 2. Prep BISP data
    # Load, convert to geopandas, extract dtl tile
    bisp_df = pd.read_csv(os.path.join(cf.SECURE_DATA_DIRECTORY, 'Data', 'BISP', 'FinalData - PII', 'GPS_uid_crosswalk.csv'))
    bisp_gdf = pd_to_gdp(bisp_df)
    bisp_gdf = gpd.sjoin(bisp_gdf, dtl_tiles, how="inner", op='intersects').reset_index(drop=True)
    bisp_gdf['geometry'] = bisp_gdf.buffer(distance = 0.75/111.12).envelope
    
    # 3. Extract DTL to BISP Coordinates

    # Load CNN parameters 
    with open(cf.CNN_PARAMS_FILENAME, 'r') as fp:
        cnn_param_dict = json.load(fp)

    # Extract
    bisp_gdf_s = bisp_gdf.head(10)
    import feature_extraction as fe
    DTL, bisp_gdf_processed = fe.map_DTL_NTL(input_gdf = bisp_gdf_s, 
                                        directory = cf.DTL_DIRECTORY, 
                                        bands = cnn_param_dict['bands'], 
                                        img_height = cnn_param_dict['image_height'], 
                                        img_width = cnn_param_dict['image_width'])



     ## DONE!   


    out_img = DTL[1,2]
    show(out_img)

    # Remove border b/c of weird zero issue?
    out_img = np.delete(out_img,  1, axis=1)
    out_img = np.delete(out_img, -1, axis=1)
    out_img = np.delete(out_img, -1, axis=2)
    out_img = np.delete(out_img, -1, axis=2)




    h, w, c = 25, 26, 7
    DTL, processed_gdf = fe.map_DTL_NTL(gdf, DTL_DIRECTORY)
    DTL = DTL.reshape((DTL.shape[0], h, w, c)) 




    #### Load Data
    # 1. BISP Coordiantes









    df, feature_dict, DTL = load_and_clean_data()



    

    layer_name = 'dense1'
    model = load_model(cnn_filepath)
    df_features = extract_features(model, DTL, layer_name)



    #################


    # SET DIRECTORY
    os.chdir(CURRENT_DIRECTORY)
    print(f'{datetime.datetime.now()} RUNNING GRID SEARCH')

    # if not grid_ready_data:
    # LOAD CLEANED DATA, DICT OF FEATURE GROUPS, AND DTL
    print(f'{datetime.datetime.now()} 1. Loading/Cleaning Data.')
    df, feature_dict, DTL = load_and_clean_data()

    # LOAD CNN, EXTRACT FEATURES, ADD TO FEATURE GROUPS
    print(f'{datetime.datetime.now()} 2. Extracting Features.')
    layer_name = 'dense1'
    model = load_model(cnn_filepath)
    df_features = extract_features(model, DTL, layer_name)

    # USE PCA TO SELECT EXTRACTED FEATURES
    print(f'{datetime.datetime.now()} 3. Performing PCA.')
    df_features_pca = perform_pca(df_features, 10)
    feature_dict['EXTRACTED_FEATURES'] = df_features_pca.columns.to_list()
    feature_dict['EXTRACT_OSM_FB_FEATURES'] = feature_dict['EXTRACTED_FEATURES'] \
                                        + feature_dict['OSM_FB_FEATURES']

    # DEFINE FINAL DATA
    df_final = df.join(df_features_pca)
    df_final.to_pickle('fully_prepped_data.pkl')

    # OVERSAMPLE
    print(f'{datetime.datetime.now()} 4. Defining Sample.')
    x_df = pd.DataFrame(df_final.drop(labels=['uid', TARGET_NAME], axis=1))
    y_df = df_final[TARGET_NAME]
    feature_dict['ALL_FEATURES'] = x_df.columns.tolist()
    oversample = RandomOverSampler(sampling_strategy=0.65)
    x, y = oversample.fit_resample(x_df, y_df)

    # SPLIT INTO TRAIN/TEST AND NORMALIZE
    print(f'{datetime.datetime.now()} 5. Defining Training and Testing Sets.')
    x_train, x_test, y_train, y_test = train_test_split(x, y, test_size=TEST_SIZE)
    normalize(x_train, x_test)

    # TRAIN MODELS AND EXPORT ERRORS
    print(f'{datetime.datetime.now()} 6. Training Models.')
    parameters = cf.GRID_TEST_CLASS
    training_errors = train_models(parameters, x_train, y_train, feature_dict)
    training_errors.to_csv(os.path.join('output', 'errors.csv'))

    # PREDICT LABELS AND EVALUATE RESULTS
    print(f'{datetime.datetime.now()} 7. Evalating Models.')
    trained_obj_list = [f for f in os.listdir('output') if f.endswith('_trained.pkl')]
    results_df = evaluate_models(trained_obj_list, x_test, y_test, feature_dict)
    results_df.to_csv(os.path.join('output', 'results.csv')) 


if __name__ == '__main__':
    run_grid_search()
