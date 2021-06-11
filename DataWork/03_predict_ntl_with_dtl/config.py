# config.py
#
# Description:
# This file holds all static configuration variables for the Poverty-from-the-
# Sky ML pipeline.

import os
import getpass
from sklearn.tree import DecisionTreeClassifier

######################
#     FILEPATHS      #
######################

## Root Paths
username = getpass.getuser()

if(username == 'robmarty'):
    DROPBOX_DIRECTORY = '/Users/robmarty/Dropbox/World Bank/IEs/Pakistan Poverty Estimation from Satellites'
    MAIN_GIT_DIRECTORY = '/Users/robmarty/Documents/Github/Pakistan-Poverty-from-Sky'
    SECURE_DATA_DIRECTORY = '/Users/robmarty/Documents/World Bank/Pakistan Poverty from Sky'

## Dependent Paths
GIT_DIRECTORY = os.path.join(MAIN_GIT_DIRECTORY, 'DataWork', '03_analysis', 'poverty_estimation')
DTL_DIRECTORY = os.path.join(DROPBOX_DIRECTORY, 'Data', 'Landsat', 'RawData', '2014')
BISP_DATA_PATH = os.path.join(DROPBOX_DIRECTORY, 'Data', 'BISP', 'bisp_socioeconomic_satellite_panel_full_satPovNAsRemoved_1hh.csv')
VIIRS_GDF_FILEPATH = os.path.join(DROPBOX_DIRECTORY, 'Data', 'VIIRS', 'FinalData', 'viirs_annual_polygon.pkl')

#CNN_PARAMS_FILENAME = os.path.join(DROPBOX_DIRECTORY, 'Models', 'CNN', 'CNN_parameters.json')
#CNN_DIRECTORY = os.path.join(DROPBOX_DIRECTORY, 'Models', 'CNN')
CNN_DIRECTORY = os.path.join(DROPBOX_DIRECTORY, 'Data', 'CNN')
CNN_FILENAME = os.path.join(DROPBOX_DIRECTORY, 'Models', 'CNN', 'script_CNN.h5')

CURRENT_DIRECTORY = GIT_DIRECTORY

BISP_COORDS_PATH = os.path.join(SECURE_DATA_DIRECTORY, 'Data', 'BISP', 'FinalData - PII', 'GPS_uid_crosswalk.csv')
PAKISTAN_BOUNDARIES_SHAPEFILE = ""

######################
#    ML VARIABLES    #
######################

TARGET_NAME = 'in_poverty'

######################
#  BUILD CLASSIFIER #
######################

GRID_TEST_CLASS = {
    'regressors': ['GaussianNB', 'LinearSVC', 'DecisionTreeClassifier', 'BaggingClassifier',
                    'GradientBoostingClassifier', 'RandomForestClassifier',
                     'AdaBoostClassifier', 'KNeighborsClassifier'],
    'LinearSVC': [
        {'penalty': penalty, 'C': C, 'loss': loss, 'class_weight': class_weight,
        'random_state': 0} \
        for penalty in ('l2', ) \
        for C in (1e-2, ) \
        for loss in ('squared_hinge', ) \
        for class_weight in ('balanced', )
    ],
    'DecisionTreeClassifier': [
        {'criterion': criterion, 'splitter': splitter, 'max_depth': max_depth,
        'max_features': max_features, 'random_state': 0} \
        for criterion in ('gini', ) \
        for splitter in ('best', ) \
        for max_depth in (1, ) \
        for max_features in ('sqrt', ) \
    ],
    'BaggingClassifier': [
        {'n_estimators': n_estimators, 'max_features': max_features,
        'random_state': 0, 'n_jobs': -1} \
        for n_estimators in (100, ) \
        for max_features in (1, )
    ],
    'GradientBoostingClassifier': [
        {'loss': loss, 'learning_rate': rate, 'n_estimators': n_estimators,
        'criterion': criterion, 'max_features': max_features,
        'random_state': 0} \
        for loss in ('deviance', ) \
        for rate in (1e-4, ) \
        for n_estimators in (100, ) \
        for criterion in ('friedman_mse', ) \
        for max_features in ('sqrt', ) \
    ],
    'RandomForestClassifier': [
        {'n_estimators': n_estimators, 'criterion': criterion,
        'max_depth': max_depth, 'max_features': max_features, 'n_jobs': -1,
        'random_state': 0} \
        for n_estimators in (100, ) \
        for criterion in ('gini', ) \
        for max_depth in (1, ) \
        for max_features in ('sqrt', )
    ],
    'AdaBoostClassifier': [
        {'n_estimators': n_estimators, 
         'base_estimator': base_estimator,
        'random_state': 0} \
        for n_estimators in (5, 10) \
        for base_estimator in (None, 
                                DecisionTreeClassifier(max_depth=2), 
                                DecisionTreeClassifier(max_depth=5),
                                DecisionTreeClassifier(max_depth=6),
                                DecisionTreeClassifier(max_depth=10),
                                DecisionTreeClassifier(max_depth=15))
    ],
    'KNeighborsClassifier': [
        {'n_neighbors': n_neighbors} \
        for n_neighbors in (2,5,10,15, ) 
    ],
    'GaussianNB': [
        {'priors': priors, 'var_smoothing': var_smoothing} \
        for priors in (None, ) \
        for var_smoothing in (1e-9, )
    ]
}

