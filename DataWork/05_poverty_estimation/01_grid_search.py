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

from sklearn.preprocessing import KBinsDiscretizer
from sklearn.model_selection import train_test_split, KFold
from sklearn.metrics import classification_report, confusion_matrix

import logging, os 

### User Defined Libraries ###
import config as cf
import feature_extraction as fe

df = pd.read_csv(os.path.join(DROPBOX_DIRECTORY, 'Data', 'BISP', 'FinalData', 'Merged Datasets', 'cnn_merge.Rds'))




