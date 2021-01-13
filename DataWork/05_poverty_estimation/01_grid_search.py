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

from sklearn.preprocessing import KBinsDiscretizer, normalize
from sklearn.model_selection import train_test_split, KFold
from sklearn.metrics import classification_report, confusion_matrix

from sklearn.linear_model import Ridge

import logging, os 

np.random.seed(42)

### User Defined Libraries ###
import config as cf
import feature_extraction as fe

# PARAMETERS
TEST_SIZE = 0.2

# Load/Prep Data ------------------------------------------
df = pd.read_csv(os.path.join(cf.DROPBOX_DIRECTORY, 'Data', 'BISP', 'FinalData', 'Merged Datasets', 'cnn_cont_merge.csv'))
df = df[df.year == 2014]

# X/Y Data ------------------------------------------------
target = 'asset_index_additive'

#x = df.filter(regex=r'^b|^viirs_buff5', axis=1)
x = df.filter(regex=r'^b|^cnn_|viirs_buff5km_year2014_spatialMEAN_monthlyMEAN', axis=1)
y = df[target]

x = normalize(x)

x_train, x_test, y_train, y_test = train_test_split(x, y, test_size=TEST_SIZE)

clf = Ridge(alpha=1)
clf.fit(x_train, y_train)

y_predict = clf.predict(x_test)

df_out = pd.DataFrame({'y_predict' : y_predict, 'y' : y_test})

df_out.corr(method='pearson')

df_out.plot.scatter('y', 'y_predict')











