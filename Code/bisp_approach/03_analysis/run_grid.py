# run_grid.py
#
# Description
# This is a script version of 03_predict_first_pass.ipynb that can be run on
# a server.

import os
import math
import pickle
import numpy as np
import pandas as pd

from sklearn.model_selection import train_test_split
from sklearn.linear_model import (LinearRegression,
                                  Lasso,
                                  Ridge)
from sklearn.svm import LinearSVR
from sklearn.tree import DecisionTreeRegressor
from sklearn.ensemble import (BaggingRegressor,
                              GradientBoostingRegressor,
                              RandomForestRegressor)
from sklearn.metrics import r2_score

import config as cf
from ml_utils import TrainedRegressor


def select_data(df):
    '''
    Takes a pandas DataFrame as input and performs the following steps:
        1. Selects data from the appropriate years
        2. Drops columns where the label is missing
    '''

    # Keep only 2011 columns, but include viirs_2012
    df = df.filter(regex='_2011', axis=1).join(df['viirs_2012'])

    # Drop columns where the label is missing
    df = df.loc[~pd.isnull(df['hhinc_2011'])]

    return df


def preprocess_data(df):
    '''
    Takes a pandas DataFrame as input and performs the following steps:
        1. Imputes missing numeric data with the column mean
        2. Creates a boolean feature for each feature that has imputed data
    '''

    for i in df.columns:
        if df[i].isnull().sum():
            # Create imputed flag
            new_name = df[i].name + '_imputed'
            df[new_name] = pd.isnull(df[i]).astype('int')
            # Fill with mean
            df[i] = df[i].fillna(df[i].mean())
        else:
            continue

    return df


def create_features(df):
    '''
    Takes a pandas DataFrame as input and performs the following steps:
        1. Creates ratios from Landsat band pairs
    '''

    for i in range(1, 8):
        for j in range(1, 8):

            if i >= j:
                continue
            else:
                band1 = f'l7_2011_{i}'
                band2 = f'l7_2011_{j}'
                new_var = f'ratio_{i}_{j}'
                df[new_var] = abs((df[band1] - df[band2]) / (df[band1] + df[band2]))

    return df


def train_models(params, features, labels, feature_sets):
    '''
    Input:  params - dictionary of model parameters
            features - dataframe of feature data
            labels - dataframe of labels
            feature_sets - dictionary of string lists of feature names
    Output: list of trained TrainedRegressor objects
    '''

    trained_list = []
    error_df = pd.DataFrame()
    count = 0

    # Loop over models, hyperparameter combinations, and feature sets
    for i in params['regressors']:
        for j in params[i]:
            for k in feature_sets:

                count += 1
                print(f'Model {count}: Training {i} on {k} with params {str(j)}')

                try:
                    # Initialize regressor, fit data, then append TrainedRegressor object to list
                    regressor = eval(i)(**j)
                    selected_features = feature_sets[k]
                    trained = regressor.fit(features[selected_features], labels)
                    trained_list.append(TrainedRegressor(i, str(j), k, trained))
                except Exception as e:
                    print(f"    ERROR: {str(e)}")
                    error_df.append({
                        'regressor': i,
                        'params': str(j),
                        'features': k,
                        'error_message': str(e)
                    }, ignore_index=True)

    return trained_list, error_df


def save_to_file(obj, path):

    # Save passed obj as a pickle to given filepath
    with open(path, 'wb') as f:
        pickle.dump(obj=obj,
                    file=f,
                    protocol=pickle.HIGHEST_PROTOCOL)
    print(f"Saving data to {path}")

    return None


def evaluate_models(models_list, test_features, test_labels, feature_sets):

    results_df = pd.DataFrame()
    for i in models_list:

        # Get predicted results from test data
        features = feature_sets[i.features]
        pred_labels = i.regressor.predict(test_features[features])

        # Append results to dataframe and sort by R^2
        pred_dict = {
            'regressor': i.method,
            'features': i.features,
            'params': i.params,
            'r2': r2_score(y_true=test_labels, y_pred=pred_labels)
        }

        results_df = results_df.append(pred_dict, ignore_index=True) \
            .sort_values(by='r2', ascending=False, axis=0) \
            [['regressor', 'params', 'features', 'r2']]

    return results_df


def main():

    # LOAD DATA
    df = pd.read_csv(cf.CLEAN_DATA_PATH)

    # SELECT APPROPRIATE COLUMNS AND ROWS
    df = select_data(df)

    # SPLIT DATA INTO TEST/TRAIN
    x_df = df.drop(labels=[cf.LABEL], axis=1)
    y_df = df[cf.LABEL]
    x_train, x_test, y_train, y_test = \
        train_test_split(x_df, y_df, test_size=cf.TEST_SIZE)

    # PREPROCES DATA
    for df in (x_train, x_test):
        preprocess_data(df)

    # CREATE FEATURES
    for df in (x_train, x_test):
        create_features(df)

    # DEFINE FEATURE GROUPS
    feature_dict = {
        'DAY_FEATURES': df.filter(regex='l7|ratio', axis=1).columns.tolist(),
        'NIGHT_FEATURES': ['dmspols_2011', 'viirs_2012', 'dmspols_2011_imputed',
                           'viirs_2012_imputed'],
        'ALL_FEATURES': df.columns.tolist()
    }

    # TRAIN MODELS AND EXPORT ERRORS
    parameters = cf.GRID_MAIN
    trained_list, training_errors = train_models(parameters, x_train, y_train, feature_dict)
    save_to_file(trained_list, os.path.join('output', 'trained_models.pkl'))
    training_errors.to_csv(os.path.join('output', 'errors.csv'))

    # PREDICT LABELS AND EVALUATE RESULTS
    results_df = evaluate_models(trained_list, x_test, y_test, feature_dict)
    save_to_file(results_df, os.path.join('output', 'results_data.pkl'))
    results_df.to_csv(os.path.join('output', 'results.csv'))


if __name__ == '__main__':
    main()
