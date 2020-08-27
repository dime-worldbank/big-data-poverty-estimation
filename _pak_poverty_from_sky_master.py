# config.py
#
# Description:
# This file holds all static configuration variables for the Poverty-from-the-
# Sky ML pipeline.

import getpass

######################
#     FILEPATHS      #
######################

## Root Paths
username = getpass.getuser()

if(username == 'robmarty'):
    DROPBOX_DIRECTORY = '/Users/robmarty/Dropbox/World Bank/IEs/Pakistan Poverty Estimation from Satellites'
    GIT_DIRECTORY = '/Users/robmarty/Documents/Github/Pakistan-Poverty-from-Sky'

## Other Paths
#CURRENT_DIRECTORY = '/Users/nguyenluong/wb_internship/Test_Run/'
#GIT_DIRECTORY = '/Users/nguyenluong/wb_internship/Pakistan-Poverty-from-Sky/DataWork/03_analysis/poverty_estimation'
#DTL_DIRECTORY = os.path.join('/Users/nguyenluong/wb_internship/Data/', 'satellite_raw', 'Landsat', '2014')
#DATA_FILEPATH = os.path.join('/Users/nguyenluong/wb_internship/Data/', 'BISP', 'bisp_socioeconomic_satellite_panel_full_satPovNAsRemoved_1hh.csv')
#BISP_COORDS_FILEPATH = os.path.join('/Users/nguyenluong/wb_internship/Data/', 'BISP/GPS_uid_crosswalk.dta')
#VIIRS_GDF_FILEPATH = os.path.join('/Users/nguyenluong/wb_internship/Data/', 'saved_objects/viirs_gdf.pkl')
#PAKISTAN_BOUNDARIES_SHAPEFILE = os.path.join('/Users/nguyenluong/wb_internship/Data/', 'pakistan_boundaries.json')

######################
#     Libraries      #
######################

## Main libraries
import os, datetime
import numpy as np
import pandas as pd
import geopandas as gpd
import random

import tensorflow as tf
from sklearn.tree import DecisionTreeClassifier
from sklearn.preprocessing import KBinsDiscretizer
from sklearn.model_selection import train_test_split, KFold
from sklearn.metrics import classification_report, confusion_matrix
from keras.utils import to_categorical
from keras.models import Sequential
from keras.layers import Conv2D, MaxPooling2D, Flatten, Dense
from keras.callbacks import EarlyStopping, ModelCheckpoint
from keras.models import load_model

## User defined libraries
os.chdir(os.path.join(GIT_DIRECTORY, 'DataWork', '03_analysis', 'poverty_estimation'))
import config as cf
import feature_extraction as fe

######################
#     Set Seeds      #
######################

seed_value = 42

# 1. Set the `PYTHONHASHSEED` environment variable at a fixed value
os.environ['PYTHONHASHSEED'] = str(seed_value)

# 2. Set the `python` built-in pseudo-random generator at a fixed value
random.seed(seed_value)

# 3. Set the `numpy` pseudo-random generator at a fixed value
np.random.seed(seed_value)

# 4. Set the `tensorflow` pseudo-random generator at a fixed value
tf.random.set_seed(seed_value)
