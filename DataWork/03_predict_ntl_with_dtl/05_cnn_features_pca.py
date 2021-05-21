# run_grid.py
#
# Description:
# Performs complete process of cleaning data and prepping all necessary data 
# and running the grid search.

# https://towardsdatascience.com/extract-features-visualize-filters-and-feature-maps-in-vgg16-and-vgg19-cnn-models-d2da6333edd0


import os
import numpy as np
import pandas as pd

from sklearn.decomposition import PCA

## User Defined
import config as cf

def perform_pca_n(df, n):
    '''
    Performs PCA with n compponents on all columns in df.
    '''
    pca = PCA(n_components=n)
    pca.fit(df)
    features_pca = pca.transform(df)
    column_names = ['cnn_pc_%01d' %i for i in range(0,n)]
    df_features_pca = pd.DataFrame(data=features_pca, columns=column_names)
    return df_features_pca, pca.explained_variance_ratio_

def perform_pca_expln(df, prop):
    '''
    Performs PCA with n compponents on all columns in df.
    '''
    pca = PCA(prop)
    pca.fit(df)
    features_pca = pca.transform(df)

    n = features_pca.shape[1]

    column_names = ['cnn_pc_%01d' %i for i in range(0,n)]
    df_features_pca = pd.DataFrame(data=features_pca, columns=column_names)
    
    return df_features_pca, pca.explained_variance_ratio_

def extract_pca_features():

    n_components = 10

    # 1. Load Data
    df = pd.read_pickle(os.path.join(cf.DROPBOX_DIRECTORY, 'Data', 'OPM' , 'FinalData', 'Individual Datasets', 'bisp_cnn_features_all.pkl'))

    # 2.Remove unique ID
    cnn_df = df.drop(['uid'], axis=1)

    # 3. PCA Dataframe
    #pca_df, expl_var = perform_pca_n(cnn_df, 20)
    pca_df, expl_var = perform_pca_expln(cnn_df, 0.9)

    # 4. Add uid 
    pca_df['uid'] = df.uid
               
    # 5. Export           
    pca_df.to_pickle(os.path.join(cf.DROPBOX_DIRECTORY, 'Data', 'OPM' , 'FinalData', 'Individual Datasets', 'bisp_cnn_features_pca.pkl'))
    pca_df.to_csv(os.path.join(cf.DROPBOX_DIRECTORY, 'Data', 'OPM' , 'FinalData', 'Individual Datasets', 'bisp_cnn_features_pca.csv'))

if __name__ == '__main__':
    extract_pca_features()
