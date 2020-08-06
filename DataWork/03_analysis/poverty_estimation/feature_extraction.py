### Helper Functions for Feature Extraction
### By: Nguyen Luong 
### Updated: 8/4/20

import os
import pandas as pd
import numpy as np
import rasterio 
from rasterio.mask import mask
from keras.models import Model


def format_coords(Polygon):
    '''
    Format coordinates of a Polygon to what rasterio likes for masking/cropping.
    
    Inputs:
        Polygon (shapely.Polygon object)
    Returns:
        (list of dictionary)
    '''
    coords = list(Polygon.exterior.coords)
    dictionary = {'type': 'Polygon'}
    dictionary['coordinates'] = [[list(tup) for tup in coords]]
    return [dictionary]


def crop(raw_DTL, polygon):
    '''
    Crops DLT according to VIIRS polygon.
    
    Inputs:
        raw_DTL (numpy.ndarray)
        polygon (shapely.Polygon object)
    Returns:
        out_img (numpy.ndarray) croppped DTL array
    '''
    shapes = format_coords(polygon)
    out_img, out_transform = mask(raw_DTL, shapes=shapes, crop=True)
    return out_img

    
def get_DTL(row, directory):
    '''
    For a given VIIRS observation, grab and crop corresponding DLT data.
    
    Inputs:
        row (pandas.Series)
        directory (str) 
    Returns: 
        all_bands: (list) list of 7 arrays, each array is 3D
    '''
    all_bands = []
    bands = [*range(1,8)]
    for b in bands:
        tile = row['tile_id']
        polygon = row['geometry']
        filename = 'l8_2014_tile{}_b{}.tif'.format(str(int(tile)), str(b))
        filepath = os.path.join(directory, filename)
        raw_DTL = rasterio.open(filepath)
        cropped_DTL = crop(raw_DTL, polygon)
        if cropped_DTL.shape == (1, 25, 26):
            all_bands.append(cropped_DTL)
    return all_bands


def map_DTL_NTL(NTL_gdf, directory):
    '''
    Gets DTL images, crops them, create arrays representing DLT and NLT to become features and targets respectively.
    
    Inputs:
        NTL_df (pandas.DataFrame)
        source, year (str)
    Returns: 
        (5D numpy.ndarray) DTL features
        (geopandas GeoDataFrame) NTL_gdf with same observations as NTL target array
    '''
    rv = []
    gdf = NTL_gdf.copy()
    for index, row in gdf.iterrows():
        DTL = get_DTL(row, directory)
        if DTL:
            # if DTL not an empty list ie if images for this tile are shape (1, 25, 26)
            rv.append(DTL)
        else:
            # remove corresponding obs from NTL data (targets)
            gdf = gdf.drop([index])
    return np.stack(rv), gdf


def extract_features(model, data, layer_name):
    # Generate feature extractor using trained CNN
    feature_extractor = Model(inputs=model.inputs,
                              outputs=model.get_layer(name=layer_name).output,)
    # Extract features and concert from tensor to numpy array
    features = feature_extractor(data).numpy()
    # Create and format pandas DataFrame
    df = pd.DataFrame(features).add_prefix('feat_')
    return df
    