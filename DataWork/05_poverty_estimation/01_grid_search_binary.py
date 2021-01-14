# 00_prep_data_for_cnn.py

# Prepares data for CNN. 
# 1. Outputs numpy arrays of DTL values and NTL labels
# 2. Creates parameter dictionary (eg, number of NTL labels)

### Libraries ###
import os, datetime
import numpy as np
import pandas as pd
import geopandas as gpd
import json
import rasterio
from rasterio.plot import show
import matplotlib.pyplot as plt

from sklearn.preprocessing import KBinsDiscretizer, StandardScaler, normalize
from sklearn.model_selection import train_test_split, KFold
from sklearn.metrics import classification_report, confusion_matrix
from sklearn.linear_model import Ridge

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
#from imblearn.over_sampling import RandomOverSampler

import logging, os 

np.random.seed(42)

### User Defined Libraries ###
import grid_params as grids
import config as cf
#import feature_extraction as fe

# PARAMETERS
TEST_SIZE = 0.2

# Functions -----------------------------------------------
def train_models(params, x_train, x_test, y_train, y_test, verbose=False):
    '''
    Saves a .pkl file of TrainedRegressor objects for each model type, as
    AWS free tier server will usually not hold all 800+ objects in memory.

    Input:  params - dictionary of model parameters
            features - dataframe of feature data
            labels - dataframe of labels
            feature_sets - dictionary of string lists of feature names
    Output: dataframe of training errors
            Also saves a .pkl file of TrainedRegressor objects for each model
    '''
    count = 0

    # Loop over models, hyperparameter combinations, and feature sets
    # Save one set of trained models for each regressor
    results_df = pd.DataFrame()
    y_df = pd.DataFrame({'y': y_test})

    for i in params['regressors']:
        models = []
        for j in params[i]:

            count += 1
            if verbose:
                print(f'{datetime.datetime.now()} Model {count}: Training {i} with params {str(j)}')
            try:
                ### Initialize regressor, fit data, then append model to list
                regressor = eval(i)(**j)
                trained = regressor.fit(x_train, y_train)
                #models.append(TrainedRegressor(i, str(j), k, trained))

                ### Results
                y_pred = trained.predict(x_test)

                pred_dict = {
                    'regressor': i,
                    'params': j,
                    'accuracy_score': accuracy_score(y_test, y_pred),
                    'recall_score': recall_score(y_test, y_pred),
                    'precision_score': precision_score(y_test, y_pred),
                    'y_truth_1': sum(y_test == 1),
                    'y_truth_0': sum(y_test == 0)
                 }

                results_df = results_df.append(pred_dict, ignore_index=True)
                y_df['y_pred_' + str(count)] = y_pred

            except Exception as e:
                print(f"{datetime.datetime.now()}    ERROR: {str(e)}")
                training_error_df.append({
                    'regressor': i,
                    'params': str(j),
                    'error_message': str(e)
                }, ignore_index=True)

    return results_df, y_df

# Load/Prep Data ------------------------------------------
df_cont = pd.read_csv(os.path.join(cf.DROPBOX_DIRECTORY, 'Data', 'BISP', 'FinalData', 'Merged Datasets', 'cnn_cont_merge.csv'))
df_cont = df_cont[df_cont.year == 2014]

df_bin = pd.read_csv(os.path.join(cf.DROPBOX_DIRECTORY, 'Data', 'BISP', 'FinalData', 'Merged Datasets', 'cnn_merge.csv'))
df_bin = df_bin[df_bin.year == 2014]

# X/Y Data ------------------------------------------------
target = 'pscores_poor'

count = 1
for target in ['pscores_poor', 'asset_tv', 'asset_washing_machinedryer', 'pscores_poor_med', 'asset_index_additive_bin', 'asset_index_pca1_bin']:
    for features_regex in ['^cnn_', '^b', '^b|viirs_buff5km_year2014_spatialMEAN_monthlyMEAN', '^b|^cnn_|viirs_buff5km_year2014_spatialMEAN_monthlyMEAN']:
        for cnn_type in ['binary', 'continuous']:

            print(target + ' ' + cnn_type + ' ' + features_regex)

            if cnn_type == 'binary':
                df = df_bin.copy()
            if cnn_type == 'continuous':
                df = df_cont.copy()

            x = df.filter(regex=features_regex, axis=1)
            y = df[target]

            x_train, x_test, y_train, y_test = train_test_split(x, y, test_size=TEST_SIZE)

            # Normalize
            x_scaler = StandardScaler().fit(x_train)

            x_train = x_scaler.transform(x_train)
            x_test = x_scaler.transform(x_test)

            # Train/Evaluate -------------------------------------------
            parameters = grids.GRID_CLASS

            r_df, pred_df = train_models(parameters, x_train, x_test, y_train, y_test, verbose=False)
            
            r_df['target'] = target
            r_df['features_regex'] = features_regex
            r_df['cnn_type'] = cnn_type
            r_df.to_csv(os.path.join(cf.DROPBOX_DIRECTORY, 'Data', 'Poverty Estimation Results', 'binary_classification', 'individual_files', 'results_' + str(count) + '.csv'))

            pred_df['target'] = target
            pred_df['features_regex'] = features_regex
            pred_df['cnn_type'] = cnn_type
            pred_df.to_csv(os.path.join(cf.DROPBOX_DIRECTORY, 'Data', 'Poverty Estimation Results', 'binary_classification', 'predicted_values', 'results_' + str(count) + '.csv'))

            count = count + 1






















