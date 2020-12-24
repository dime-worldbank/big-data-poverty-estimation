# ml_utils.py
#
# Description:
# Defines useful functions and classes used in the ML pipeline.

# Define a TrainedRegressor object to hold key results information
class TrainedRegressor:

    def __init__(self, method, params, features, regressor):
        self.method = method
        self.params = params
        self.features = features
        self.regressor = regressor

    def __repr__(self):
        return f'Trained {self.method} on feature set {self.features} with params {self.params}'
