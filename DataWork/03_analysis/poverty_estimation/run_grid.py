# run_grid.py
#
# Description
# This is a script version of 03_predict_first_pass.ipynb that can be run on
# a server.

import os, math, pickle, datetime
import numpy as np
import pandas as pd

from sklearn.preprocessing import StandardScaler
from sklearn.model_selection import train_test_split
from sklearn.decomposition import PCA

from sklearn.svm import LinearSVC
from sklearn.tree import DecisionTreeClassifier
from sklearn.ensemble import (BaggingClassifier, AdaBoostClassifier, 
                              GradientBoostingClassifier, RandomForestClassifier)
from sklearn.neighbors import KNeighborsClassifier
from sklearn.naive_bayes import GaussianNB
from sklearn.metrics import (accuracy_score, precision_score, 
                             recall_score, classification_report)
from keras.models import load_model
from imblearn.over_sampling import RandomOverSampler

import logging 
logging.disable(logging.WARNING) 
os.environ["TF_CPP_MIN_LOG_LEVEL"] = "3"

import warnings
warnings.filterwarnings('ignore')

import config as cf
from feature_extraction import extract_features
from clean_data import load_and_clean_data
from ml_utils import TrainedRegressor

# Must define
CURRENT_DIRECTORY = "/Users/nguyenluong/wb_internship/Data/"
CNN_FILENAME = 'script_CNN.h5'

TARGET_NAME = 'in_poverty'
TEST_SIZE = 0.2


def save_to_file(obj, path):
    '''
    Saves passed obj as a pickle to given filepath.
    '''
    with open(path, 'wb') as f:
        pickle.dump(obj=obj,
                    file=f,
                    protocol=pickle.HIGHEST_PROTOCOL)
    print(f"{datetime.datetime.now()} Saving data to {path}")

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
            recall_nonpoverty = cr['not_pov']['recall']
            recall_poverty = cr['pov']['recall']
            recall_diff = abs(recall_poverty - recall_nonpoverty)

            pred_dict = {
                'regressor': i.method,
                'params': i.params,
                'features': i.features,
                'accuracy_score': accuracy_score(test_labels, pred_labels),
                'recall_score': recall_score(test_labels, pred_labels),
                'precision_score': precision_score(test_labels, pred_labels),
                'nonpoverty_class': cr['not_pov'],
                'poverty_class': cr['pov'],
                'recall_nonpoverty': recall_nonpoverty,
                'recall_poverty': recall_poverty,
                'score': max(recall_nonpoverty, recall_poverty) - recall_diff
            }
    
            results_df = results_df.append(pred_dict, ignore_index=True)
            results_df = results_df.reindex(
                ['regressor', 'params', 'features', 'accuracy_score', 'recall_score', 
                 'precision_score', 'nonpoverty_class', 'poverty_class', 'recall_nonpoverty', 
                 'recall_poverty', 'score'], axis=1
            )

    return results_df


def main():

    # SET DIRECTORY
    os.chdir(CURRENT_DIRECTORY)
    print(f'{datetime.datetime.now()} 1. Directory set.')

    # LOAD CLEANED DATA, DICT OF FEATURE GROUPS, AND DTL
    df, feature_dict, DTL = load_and_clean_data()
    print(f'{datetime.datetime.now()} 2. Clean data, feature groups, and DTL loaded.')
    df.to_pickle('script_fully_prepped.pkl')

    # LOAD CNN, EXTRACT FEATURES, ADD TO FEATURE GROUPS
    layer_name = 'dense1'
    model = load_model(CNN_FILENAME)
    df_features = extract_features(model, DTL, layer_name)
    print(f'{datetime.datetime.now()} 3. Features extracted.')

    # USE PCA TO SELECT EXTRACTED FEATURES
    df_features_pca = perform_pca(df_features, 10)
    feature_dict['EXTRACTED_FEATURES'] = df_features_pca.columns.to_list()
    df_final = df.join(df_features_pca)
    print(f'{datetime.datetime.now()} 4. PCA performed.')

    # OVERSAMPLE
    x_df = pd.DataFrame(df_final.drop(labels=['uid', TARGET_NAME], axis=1))
    y_df = df_final[TARGET_NAME]
    oversample = RandomOverSampler(sampling_strategy=0.75, random_state=1)
    x, y = oversample.fit_resample(x_df, y_df)
    print(f'{datetime.datetime.now()} 5. Oversampling performed.')

    # SPLIT INTO TRAIN/TEST AND NORMALIZE
    x_train, x_test, y_train, y_test =  train_test_split(x, y, test_size=TEST_SIZE, random_state=1)
    normalize(x_train, x_test)
    print(f'{datetime.datetime.now()} 6. Training and Testing sets defined.')

    # TRAIN MODELS AND EXPORT ERRORS
    parameters = cf.GRID_TEST_CLASS
    training_errors = train_models(parameters, x_train, y_train, feature_dict)
    training_errors.to_csv(os.path.join('output', 'errors.csv'))
    print(f'{datetime.datetime.now()} 7. Models trained.')

    # PREDICT LABELS AND EVALUATE RESULTS
    trained_obj_list = [f for f in os.listdir('output') if f.endswith('_trained.pkl')]
    results_df = evaluate_models(trained_obj_list, x_test, y_test, feature_dict)
    results_df.to_csv(os.path.join('output', 'results.csv'))
    print(f'{datetime.datetime.now()} 8. Results saved.')


if __name__ == '__main__':
    main()
