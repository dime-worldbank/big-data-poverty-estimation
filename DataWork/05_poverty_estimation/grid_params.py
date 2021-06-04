import numpy as np
from sklearn.tree import DecisionTreeClassifier, DecisionTreeRegressor


# Regressors -------------------------------------------------------
GRID_REGRESS = {
    'regressors': ['Ridge', 'Lasso', 'ElasticNet', 'LinearSVR','DecisionTreeRegressor', 'BaggingRegressor',
                   'GradientBoostingRegressor','RandomForestRegressor','AdaBoostRegressor',
                  'KNeighborsRegressor'],
    'Ridge': [
        {'alpha': alpha} \
        for alpha in (2**np.arange(-10, 40, 0.5))
    ],
    'Lasso': [
        {'alpha': alpha}\
        for alpha in (2**np.arange(-10, 40, 0.5))
    ],
    'ElasticNet': [
      {'alpha':alpha, 'l1_ratio':ratio}
        for alpha in (2**np.arange(-10, 40, 0.5)) \
        for ratio in (np.arange(0,1.25,.25))
    ],
    'LinearSVR': [
        {'C': C, 'loss': loss,
        'random_state': 0} \
        for C in (np.arange(0.001, 1.01, .01)) \
        for loss in ('epsilon_insensitive', )
    ],
     'DecisionTreeRegressor': [
        {'criterion': criterion, 'splitter': splitter, 'max_depth': max_depth,
        'max_features': max_features, 'random_state': 0} \
        for criterion in ('mse','friedman_mse' ) \
        for splitter in ('best','random', ) \
        for max_depth in (np.arange(1,5,1)) \
        for max_features in ('sqrt', 'auto', 'log2') \
    ],
    'BaggingRegressor': [
        {'n_estimators': n_estimators, 'max_features': max_features,
        'random_state': 0, 'n_jobs': -1} \
        for n_estimators in (np.arange(10,110,10)) \
        for max_features in (np.arange(5,50,5))
    ],
        'GradientBoostingRegressor': [
        {'loss': loss, 'learning_rate': rate, 'n_estimators': n_estimators,
        'criterion': criterion, 'max_features': max_features,
        'random_state': 0} \
        for loss in ('ls', ) \
        for rate in (np.arange(0.001, .01, .001)) \
        for n_estimators in (np.arange(80,110,10)) \
        for criterion in ('friedman_mse', ) \
        for max_features in ('auto','log2','sqrt', ) 
     ],
    'RandomForestRegressor': [
        {'n_estimators': n_estimators, 'criterion': criterion,
             'max_features': max_features, 'n_jobs': -1,
        'random_state': 0} \
        for n_estimators in (np.arange(50,110,10)) \
        for criterion in ('mse', ) \
        for max_features in ('auto','log2','sqrt', )
    ],
    'AdaBoostRegressor': [
        {'n_estimators': n_estimators, 
         'base_estimator': base_estimator, 'learning_rate': rate,
        'random_state': 0} \
        for n_estimators in (np.arange(30,80,10)) \
        for base_estimator in (None, 
                                DecisionTreeRegressor(max_depth=2), 
                                DecisionTreeRegressor(max_depth=5),
                                DecisionTreeRegressor(max_depth=6),
                                DecisionTreeRegressor(max_depth=10),
                                DecisionTreeRegressor(max_depth=15)) \
        for rate in (np.arange(.5,2.5,.5))
    ],
    'KNeighborsRegressor': [
        {'n_neighbors': n_neighbors, 'algorithm': algorithm, 'n_jobs': n_jobs} \
        for n_neighbors in (np.arange(1,15,1)) \
        for algorithm in ('auto', 'ball_tree', 'kd_tree', 'brute') \
        for n_jobs in (1, -1, )
    ]
}


# Classifiers -------------------------------------------------------
GRID_CLASS = {
    'regressors': ['GaussianNB', 'LinearSVC', 'DecisionTreeClassifier', 'BaggingClassifier',
                    'GradientBoostingClassifier', 'RandomForestClassifier',
                     'AdaBoostClassifier', 'KNeighborsClassifier'],
    'LinearSVC': [
        {'penalty': penalty, 'C': C, 'loss': loss, 'class_weight': class_weight,
        'random_state': 0} \
        for penalty in ('l2', ) \
        for C in (np.arange(0.001, 1.01, .01)) \
        for loss in ('squared_hinge', ) \
        for class_weight in ('balanced', )
    ],
    'DecisionTreeClassifier': [
        {'criterion': criterion, 'splitter': splitter, 'max_depth': max_depth,
        'max_features': max_features, 'random_state': 0} \
        for criterion in ('gini', 'entropy') \
        for splitter in ('best', 'random', ) \
        for max_depth in (np.arange(1,5,1)) \
        for max_features in ('sqrt', 'auto', 'log2') \
    ],
    'BaggingClassifier': [
        {'n_estimators': n_estimators, 'max_features': max_features,
        'random_state': 0, 'n_jobs': -1} \
        for n_estimators in (np.arange(10,110,10)) \
        for max_features in (np.arange(5,50,5))
    ],
    'GradientBoostingClassifier': [
        {'loss': loss, 'learning_rate': rate, 'n_estimators': n_estimators,
        'criterion': criterion, 'max_features': max_features,
        'random_state': 0} \
        for loss in ('deviance', ) \
        for rate in (np.arange(0.001, .01, .001)) \
        for n_estimators in (np.arange(80,110,10)) \
        for criterion in ('friedman_mse', ) \
        for max_features in ('auto','log2','sqrt',) 
    ],
    'RandomForestClassifier': [
        {'n_estimators': n_estimators, 'criterion': criterion,
         'max_features': max_features, 'n_jobs': -1,
        'random_state': 0} \
        for n_estimators in (np.arange(10,110,10)) \
        for criterion in ('gini', ) \
        for max_features in ('auto','log2','sqrt', )
    ],
    'AdaBoostClassifier': [
        {'n_estimators': n_estimators, 
         'base_estimator': base_estimator, 'learning_rate': rate,
        'random_state': 0} \
        for n_estimators in (np.arange(30,80,10)) \
        for base_estimator in (None, 
                                DecisionTreeClassifier(max_depth=2), 
                                DecisionTreeClassifier(max_depth=5),
                                DecisionTreeClassifier(max_depth=6),
                                DecisionTreeClassifier(max_depth=10),
                                DecisionTreeClassifier(max_depth=15)) \
        for rate in (np.arange(.5,2.5,.5))
    ],
    'KNeighborsClassifier': [
        {'n_neighbors': n_neighbors, 'algorithm': algorithm, 'n_jobs': n_jobs} \
        for n_neighbors in (np.arange(1,15,1)) \
        for algorithm in ('auto', 'ball_tree', 'kd_tree', 'brute') \
        for n_jobs in (1, -1, )
    ],
     'GaussianNB': [
        {'priors': priors, 'var_smoothing': var_smoothing} \
        for priors in (None, ) \
        for var_smoothing in (1e-9, 2e-9, 3e-9,)
    ]
}

