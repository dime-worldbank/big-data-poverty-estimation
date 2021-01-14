
from sklearn.tree import DecisionTreeClassifier, DecisionTreeRegressor

# Regressors -------------------------------------------------------
GRID_REGRESS = {
    'regressors': ['Ridge', 'DecisionTreeRegressor', 'BaggingRegressor',
                    'GradientBoostingRegressor', 'RandomForestRegressor',
                     'AdaBoostRegressor', 'KNeighborsRegressor'],
    'Ridge': [
        {'alpha': alpha} \
        for alpha in (1, 5, 10, )
    ],
    'LinearSVC': [
        {'penalty': penalty, 'C': C, 'loss': loss, 'class_weight': class_weight,
        'random_state': 0} \
        for penalty in ('l2', ) \
        for C in (1e-2, ) \
        for loss in ('squared_hinge', ) \
        for class_weight in ('balanced', )
    ],
    'DecisionTreeRegressor': [
        {'criterion': criterion, 'splitter': splitter, 'max_depth': max_depth,
        'max_features': max_features, 'random_state': 0} \
        for criterion in ('mse', ) \
        for splitter in ('best', ) \
        for max_depth in (1, 2, 5, ) \
        for max_features in ('sqrt', ) \
    ],
    'BaggingRegressor': [
        {'n_estimators': n_estimators, 'max_features': max_features,
        'random_state': 0, 'n_jobs': -1} \
        for n_estimators in (100, ) \
        for max_features in (1, 5, 10, )
    ],
    'GradientBoostingRegressor': [
        {'loss': loss, 'learning_rate': rate, 'n_estimators': n_estimators,
        'criterion': criterion, 'max_features': max_features,
        'random_state': 0} \
        for loss in ('ls', ) \
        for rate in (1e-4, ) \
        for n_estimators in (100, ) \
        for criterion in ('friedman_mse', ) \
        for max_features in ('sqrt', ) \
    ],
    'RandomForestRegressor': [
        {'n_estimators': n_estimators, 'criterion': criterion,
        'max_depth': max_depth, 'max_features': max_features, 'n_jobs': -1,
        'random_state': 0} \
        for n_estimators in (100, ) \
        for criterion in ('mse', ) \
        for max_depth in (1, ) \
        for max_features in ('sqrt', )
    ],
    'AdaBoostRegressor': [
        {'n_estimators': n_estimators, 
         'base_estimator': base_estimator,
        'random_state': 0} \
        for n_estimators in (5, 10, 20) \
        for base_estimator in (None, 
                                DecisionTreeRegressor(max_depth=2), 
                                DecisionTreeRegressor(max_depth=5),
                                DecisionTreeRegressor(max_depth=6),
                                DecisionTreeRegressor(max_depth=10),
                                DecisionTreeRegressor(max_depth=15))
    ],
    'KNeighborsRegressor': [
        {'n_neighbors': n_neighbors} \
        for n_neighbors in (2,5,10,15, ) 
    ],
    'GaussianNB': [
        {'priors': priors, 'var_smoothing': var_smoothing} \
        for priors in (None, ) \
        for var_smoothing in (1e-9, )
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
        for max_features in (1, 5, 10, )
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
        for n_estimators in (5, 10, 20) \
        for base_estimator in (None, DecisionTreeClassifier(max_depth=2), 
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

