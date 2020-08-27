# clean_data.py
#
# Description:
# Cleans and fully preps data used in run_grid.py.

import os, datetime
import numpy as np
import pandas as pd 
import geopandas as gpd
from math import floor

import config as cf
import feature_extraction as fe

TARGET_NAME = cf.TARGET_NAME
CURRENT_DIRECTORY = cf.CURRENT_DIRECTORY
DATA_FILEPATH = cf.DATA_FILEPATH
VIIRS_GDF_FILEPATH = cf.VIIRS_GDF_FILEPATH
BISP_COORDS_FILEPATH = cf.BISP_COORDS_FILEPATH
DTL_DIRECTORY = cf.DTL_DIRECTORY


def load_data():
    '''
    Loads data and filters for 2014.
    '''
    df = pd.read_csv(DATA_FILEPATH)
    df = df[df['year'] == 2014]
    return df


def transform_target(df):
    '''
    Transforms poverty scores into binary variable.
    '''
    df[TARGET_NAME] = (df['pscores']<=16.17)
    df[TARGET_NAME] = df[TARGET_NAME].astype(int)
    return df


def get_lat_lon(number):
    '''
    Helper function to convert GPSN, GPSE iinto lat, lon.
    '''
    deg = floor(number / 100)
    min = floor(number - (100 * deg))
    sec = 100 * (number - (100 * deg) - min)
    degree = deg + (min / 60) + (sec / 3600)
    return degree


def load_and_prep_coords():
    '''
    Preps coordinate data to be added to BISP data.
    '''
    # Load coords 
    coords = pd.read_stata(BISP_COORDS_FILEPATH)
    # Drop NAs
    coords = coords[~coords['GPSN'].isna()]
    # Get lat, lon
    coords['lat'] = coords['GPSN'].apply(lambda x: get_lat_lon(x))
    coords['lon'] = coords['GPSE'].apply(lambda x: get_lat_lon(x))
    # Convert uid to integer
    coords['uid'] = coords['uid'].astype(int)
    # Create geopandas
    gdf = gpd.GeoDataFrame(coords, geometry=gpd.points_from_xy(coords['lon'], coords['lat']))
    return gdf


def load_and_clean_data():

    # SET DIRECTORY
    os.chdir(CURRENT_DIRECTORY)

    # LOAD BISP DATA AND TRANSFORM TARGET
    df = load_data()
    df = transform_target(df)
    print(f'{datetime.datetime.now()}    1.1 BISP data loaded and target transformed.')

    # LOAD AND MATCH COORDS TO BISP DATA
    coords = load_and_prep_coords()
    gdf_bisp = coords.merge(df, left_on='uid', right_on='uid')
    gdf_bisp.to_pickle('bisp_with_coords.pkl')
    print(f'{datetime.datetime.now()}    1.2 Coordinates added to BISP data.')

    # LOAD AND SPATIALLY JOIN SATELLITE DATA TO BISP DATA

    viirs = pd.read_pickle(os.path.join(DROPBOX_DIRECTORY, 'Data', 'VIIRS', 'FinalData', 'viirs_annual_polygon.pkl'))
    viirs_gdf = gpd.GeoDataFrame(viirs, geometry='geometry')
    gdf = gpd.sjoin(viirs_gdf, gdf_bisp, how="inner", op='intersects').reset_index(drop=True)
    print(f'{datetime.datetime.now()}    1.3 Satellite and BISP data spatially joined.')

    # MAP DTL IMAGE FILES TO BISP DATA
    h, w, c = 25, 26, 7
    DTL, processed_gdf = fe.map_DTL_NTL(gdf, DTL_DIRECTORY)
    DTL = DTL.reshape((DTL.shape[0], h, w, c))
    print(f'{datetime.datetime.now()}    1.4 DTL images mapped to BISP data.')

    # DEFINE FEATURE GROUPS AND FINAL DATA
    df_viirs = processed_gdf.filter(regex='viirs').filter(regex='_2km')
    df_landsat = processed_gdf.filter(regex='^b').filter(regex='_1km')
    df_osm = processed_gdf.filter(regex='fclass').filter(regex='meters')
    df_fb = processed_gdf.filter(regex='^estimate_dau')
    df_y = processed_gdf[['uid', TARGET_NAME]]
    df_final = df_y.join(df_osm).join(df_fb).join(df_landsat).join(df_viirs).reset_index(drop=True)

    # STORE FEATURE GROUPS IN DICT
    geo_features = df_viirs.columns.tolist() + df_landsat.columns.tolist()
    osm_fb_features = df_osm.columns.tolist() + df_fb.columns.tolist()

    feature_dict = {'NUMERIC_GEO_FEATURES': geo_features,
                    'OSM_FB_FEATURES': osm_fb_features}
    print(f'{datetime.datetime.now()}    1.5 Final data and features groups defined.')

    return df_final, feature_dict, DTL

