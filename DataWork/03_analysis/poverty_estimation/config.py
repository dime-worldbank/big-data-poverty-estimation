# config.py
#
# Description:
# This file holds all static configuration variables for the Poverty-from-the-
# Sky ML pipeline.

import os
from sklearn.tree import DecisionTreeClassifier

######################
#     FILEPATHS      #
######################

CURRENT_DIRECTORY = "/Users/nguyenluong/wb_internship/Data/"
VIIRS_GDF_FILEPATH = 'saved_objects/viirs_gdf.pkl'
DTL_DIRECTORY = os.path.join('satellite_raw', 'Landsat', '2014')
DATA_FILEPATH = os.path.join('BISP', 'bisp_socioeconomic_satellite_panel_full_satPovNAsRemoved_1hh.csv')
BISP_COORDS_FILEPATH = 'BISP/GPS_uid_crosswalk.dta'
PAKISTAN_BOUNDARIES_SHAPEFILE = 'pakistan_boundaries.json'

######################
#  BUILD CLASSIFIER #
######################

# Test grid to make sure everything works - limited models and parameters (classification)
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


# Main grid - most exhaustive option (classification)
GRID_MAIN_CLASS = {
    'regressors': ['LinearSVC', 'DecisionTreeClassifier', 'BaggingClassifier',
                   'AdaBoostClassifier', 'KNeighborsClassifier', 'RandomForestClassifier', 
                   'GradientBoostingClassifier', 'GaussianNB'],
    'LinearSVC': [
        {'penalty': penalty, 'C': C, 'loss': loss, 'max_iter': max_iter,
        'random_state': 0} \
        for penalty in ('l2', ) \
        for C in (1e-2,1,2) \
        for loss in ('epsilon_insensitive','squared_hinge', ) \
        for max_iter in (1e1, )
    ],
    'DecisionTreeClassifier': [
        {'criterion': criterion, 'splitter': splitter, 'max_depth': max_depth,
        'max_features': max_features, 'random_state': 0} \
        for criterion in ('gini', ) \
        for splitter in ('best', ) \
        for max_depth in (1,2,3,4, 5, 10, 20, 30, 50, 70, 100, ) \
        for max_features in ('sqrt', ) \
    ],
    'BaggingClassifier': [
        {'n_estimators': n_estimators, 'max_features': max_features,
        'random_state': 0, 'n_jobs': -1} \
        for n_estimators in (10, 50, 100, 1000,) \
        for max_features in (0.1, 0.2, 0.3,0.4, 0.5, 1.0,)
    ],
    'AdaBoostClassifier': [
        {'n_estimators': n_estimators, 
         'base_estimator': base_estimator,
        'random_state': 0} \
        for n_estimators in (5, 10, 50, 100) \
        for base_estimator in (None, 
                                DecisionTreeClassifier(max_depth=2), 
                                DecisionTreeClassifier(max_depth=5),
                                DecisionTreeClassifier(max_depth=6),
                                DecisionTreeClassifier(max_depth=10),
                                DecisionTreeClassifier(max_depth=15))
    ],
    'KNeighborsClassifier': [
        {'n_neighbors': n_neighbors} \
        for n_neighbors in (2,5,10,15,) 
    ],
    'RandomForestClassifier': [
        {'n_estimators': n_estimators, 'criterion': criterion,
        'max_depth': max_depth, 'max_features': max_features, 'n_jobs': -1,
        'random_state': 0} \
        for n_estimators in (5, 10, 100, 1000, 5000) \
        for criterion in ('gini', ) \
        for max_depth in (1,2,3,4,5,6,7,8,9,10, ) \
        for max_features in ('sqrt','log2',None, )
    ],
    'GradientBoostingClassifier': [
        {'loss': loss, 'learning_rate': rate, 'n_estimators': n_estimators,
        'criterion': criterion, 'max_features': max_features,
        'random_state': 0} \
        for loss in ('deviance', ) \
        for rate in (1e-4, )
        for n_estimators in (100, ) \
        for criterion in ('friedman_mse', ) \
        for max_features in ('sqrt', ) \
    ],
    'GaussianNB': [
        {'priors': priors, 'var_smoothing': var_smoothing} \
        for priors in (None, ) \
        for var_smoothing in (1e-9, )
    ]
}
