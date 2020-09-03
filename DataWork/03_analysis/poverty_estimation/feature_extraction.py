# feature_extraction.py
#
# Description: These are helper functions for feature extraction.
# Current author: Nguyen Luong 
# Updated: 8/13/20

import os
import pandas as pd
import numpy as np
import rasterio 
from rasterio.mask import mask
from rasterio.enums import Resampling
from keras.models import Model
from keras.applications.imagenet_utils import preprocess_input


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


def mask_and_write(filepath, polygon):
    '''
    Crops DLT according to VIIRS polygon, write out a .tif called
    'masked.tif' to be used in resampling.
    
    Inputs:
        filepath (str)
        polygon (shapely.Polygon object)
    Returns:
        None
    '''
    shapes = format_coords(polygon) 
    with rasterio.open(filepath) as dataset:
        out_img, out_transform = mask(dataset, shapes=shapes, crop=True)
        out_meta = dataset.meta

        out_img = np.delete(out_img, -1, axis=2)
    
    out_meta.update({"driver": "GTiff",
                     "height": out_img.shape[1],
                     "width": out_img.shape[2],
                     "transform": out_transform})

    with rasterio.open("masked.tif", "w", **out_meta) as dest:
        dest.write(out_img)


def resample(new_height, new_width):
    '''
    Rescale masked images to new_width and new_height.

    Inputs:
        new_width, new_height (int)
    Returns:
        data (numpy.ndarray)
    '''
    with rasterio.open('masked.tif') as dataset:
        # Resampling.nearest
        data = dataset.read(out_shape=(dataset.count, new_height, new_width),
                            resampling=Resampling.bilinear)
    
    return data

    
def get_DTL(row, directory, bands, img_height, img_width):
    '''
    For a given VIIRS observation, grab and crop corresponding DLT data.
    
    Inputs:
        row (pandas.Series)
        directory (str) 
    Returns: 
        all_bands: (list) list of 7 arrays, each array is 3D
    '''
    all_bands = []

    #bands = [*range(1,8)]
    #crop_img_height = 25
    #crop_img_width = 25

    for b in bands:
        tile, polygon = int(row['tile_id']), row['geometry']
        filename = f'l8_2014_tile{str(tile)}_b{str(b)}.tif'
        filepath = os.path.join(directory, filename)

        #shapes = format_coords(polygon) 
        #with rasterio.open(filepath) as dataset:
        #    out_img, out_transform = mask(dataset, shapes=shapes, crop=True)
        #    out_meta = dataset.meta

        mask_and_write(filepath, polygon)
        data = resample(img_height, img_width)
        all_bands.append(data)

        if data.shape != (1, img_height, img_width):
            print(f'Flag: Irregular cropped image shape {data.shape}')
            
    return all_bands


def map_DTL_NTL(input_gdf, directory, bands, img_height, img_width):
    '''
    Gets DTL images, crops them, create arrays representing DLT and NLT to 
    become features and targets respectively.
    
    Inputs:
        NTL_df (pandas.DataFrame)
        source, year (str)
        bands: list of landsat bands to use
        img_height: rescaled image height
        img_width: rescaled image width
    Returns: 
        (5D numpy.ndarray) DTL features
        (geopandas GeoDataFrame) NTL_gdf with same observations as NTL target array
    '''
    DTL_list = []
    gdf = input_gdf.copy()

    for i, row in gdf.iterrows():
        DTL = get_DTL(row, directory, bands, img_height, img_width)
        if DTL:
            # if DTL not an empty list ie if images for this tile are shape (1, 25, 26)
            DTL_list.append(DTL)
        else:
            # remove corresponding obs from NTL data (targets)
            gdf = gdf.drop([i])

    return np.stack(DTL_list), gdf


def extract_features(model, data, layer_name):
    '''
    Extracts features from satelitte image data.

    Inputs:
        model (keras.Model)
        data (numpy.ndarray)
        layer_name (str)
    Returns:
        (pandas DataFrame) features
    '''
    # Preprocess image data
    data = preprocess_input(data)

    # Generate feature extractor using trained CNN
    feature_extractor = Model(inputs=model.inputs,
                              outputs=model.get_layer(name=layer_name).output,)

    # Extract features and convert from tensor to numpy array
    features = feature_extractor(data).numpy()

    # Create and format pandas DataFrame
    df = pd.DataFrame(features).add_prefix('feat_')

    return df
    