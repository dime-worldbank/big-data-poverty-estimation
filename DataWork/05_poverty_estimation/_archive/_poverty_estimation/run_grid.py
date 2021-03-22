# run_grid.py
#
# Description:
# Performs complete process of cleaning data and prepping all necessary data 
# and running the grid search.

import os, math, pickle, datetime, json
import numpy as np
import pandas as pd

from sklearn.decomposition import PCA
from sklearn.model_selection import train_test_split
from sklearn.preprocessing import StandardScaler
from sklearn.svm import LinearSVC
from sklearn.tree import DecisionTreeClassifier, DecisionTreeRegressor
from sklearn.ensemble import (BaggingClassifier, AdaBoostClassifier, 
                              GradientBoostingClassifier, RandomForestClassifier,
                              BaggingRegressor, AdaBoostRegressor, 
                              GradientBoostingRegressor, RandomForestRegressor)
from sklearn.neighbors import KNeighborsClassifier, KNeighborsRegressor
from sklearn.naive_bayes import GaussianNB
from sklearn.metrics import (accuracy_score, precision_score, 
                             recall_score, classification_report)
from keras.models import load_model
#from imblearn.over_sampling import RandomOverSampler

import logging 
logging.disable(logging.WARNING) 
os.environ["TF_CPP_MIN_LOG_LEVEL"] = "3"
import warnings
warnings.filterwarnings('ignore')

import config as cf
import grid_params as grids
from feature_extraction import extract_features
from clean_data import load_and_clean_data
from ml_utils import TrainedRegressor

### FOR REPRODUCIBILITY ###
seed_value = 1
# 1. Set the `PYTHONHASHSEED` environment variable at a fixed value
os.environ['PYTHONHASHSEED'] = str(seed_value)
# 2. Set the `python` built-in pseudo-random generator at a fixed value
import random
random.seed(seed_value)
# 3. Set the `numpy` pseudo-random generator at a fixed value
np.random.seed(seed_value)
# 4. Set the `tensorflow` pseudo-random generator at a fixed value
import tensorflow as tf
tf.random.set_seed(seed_value)
# 5. Configure a new global `tensorflow` session
session_conf = tf.compat.v1.ConfigProto(intra_op_parallelism_threads=1, inter_op_parallelism_threads=1)
sess = tf.compat.v1.Session(graph=tf.compat.v1.get_default_graph(), config=session_conf)
tf.compat.v1.keras.backend.set_session(sess)

TEST_SIZE = 0.2
TARGET_NAME = cf.TARGET_NAME
CURRENT_DIRECTORY = cf.CURRENT_DIRECTORY


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


def train_models(params, features, labels, feature_sets, verbose=False):
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

    training_error_df = pd.DataFrame()
    count = 0

    # Loop over models, hyperparameter combinations, and feature sets
    # Save one set of trained models for each regressor
    for i in params['regressors']:
        models = []
        for j in params[i]:
            for k in feature_sets:

                count += 1
                if verbose:
                    print(f'{datetime.datetime.now()} Model {count}: Training {i} on {k} with params {str(j)}')
                try:
                    # Initialize regressor, fit data, then append model to list
                    regressor = eval(i)(**j)
                    selected_features = feature_sets[k]
                    trained = regressor.fit(features[selected_features], labels)
                    models.append(TrainedRegressor(i, str(j), k, trained))
                except Exception as e:
                    print(f"{datetime.datetime.now()}    ERROR: {str(e)}")
                    training_error_df.append({
                        'regressor': i,
                        'params': str(j),
                        'features': k,
                        'error_message': str(e)
                    }, ignore_index=True)
        save_to_file(models, os.path.join('output', i + '_trained.pkl'))

    return training_error_df


def evaluate_models(obj_list, test_features, test_labels, feature_sets):
    '''
    Input:  obj_list - string list of file names in .pkl form. Each .pkl file
                unpacks into a list of TrainedRegressor objects.
            test_features - pd DataFrame of features for test data
            test_labels - pd DataFrame of labels for test data
            feature_sets - dictionary of lists of feature string names
    '''
    results_df = pd.DataFrame()
    for obj in obj_list:

        print(f"{datetime.datetime.now()}    Evaluating {obj}")
        obj_path = os.path.join('output', obj)
        with open(obj_path, 'rb') as f:
            model_list = pickle.load(f)

        for i in model_list:

            # Get predicted results from test data
            features = feature_sets[i.features]
            pred_labels = i.regressor.predict(test_features[features])

            # Append results to dataframe
            target_names = ['not_pov', 'pov']
            cr = classification_report(test_labels, pred_labels, 
                                        target_names=target_names,
                                        zero_division=0,
                                        output_dict=True)
            nonpoverty_class = cr['not_pov']
            poverty_class = cr['pov']
            recall_nonpoverty = cr['not_pov']['recall']
            recall_poverty = cr['pov']['recall']
            recall_diff = abs(recall_poverty - recall_nonpoverty)

            pred_dict = {
                'regressor': i.method,
                'params': i.params,
                'feature_group': i.features,
                'feature_columns': features,
                'accuracy_score': accuracy_score(test_labels, pred_labels),
                'recall_score': recall_score(test_labels, pred_labels),
                'precision_score': precision_score(test_labels, pred_labels),
                'nonpoverty_class': nonpoverty_class,
                'poverty_class': poverty_class,
                'recall_nonpoverty': recall_nonpoverty,
                'recall_poverty': recall_poverty,
                'final_score': max(recall_nonpoverty, recall_poverty) - recall_diff
            }
    
            results_df = results_df.append(pred_dict, ignore_index=True)
            results_df = results_df.reindex(
                ['regressor', 'params', 'feature_group', 'feature_columns', \
                 'accuracy_score', 'recall_score', 'precision_score', 'nonpoverty_class', 
                 'poverty_class', 'recall_nonpoverty', 'recall_poverty', 'final_score'], axis=1
            )
            results_df = results_df.reindex(
                ['regressor', 'params', 'feature_group', 'feature_columns', \
                 'accuracy_score', 'recall_score', 'precision_score', 'nonpoverty_class', 
                 'poverty_class', 'recall_nonpoverty', 'recall_poverty', 'final_score'], axis=1
            )

    return results_df


def run_grid_search(cnn_filepath='script_CNN.h5', grid_ready_data=None,
                    grid_ready_feature_groups=None):

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
