# feature_extraction.py
#
# Description: These are helper functions for feature extraction.
# Current author: Nguyen Luong 
# Updated: 8/13/20

import os
import pandas as pd
import numpy as np
import cv2
#import rasterio 
#from rasterio.mask import mask
#from rasterio.enums import Resampling
from keras.models import Model
from keras.applications.imagenet_utils import preprocess_input

# https://automating-gis-processes.github.io/CSC18/lessons/L6/clipping-raster.html
def getFeatures(gdf):
    """Function to parse features from GeoDataFrame in such a manner that rasterio wants them"""
    import json
    return [json.loads(gdf.to_json())['features'][0]['geometry']]

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


def read_crop_resample_raster(filepath, polygon, img_height, img_width):
    '''
    https://automating-gis-processes.github.io/CSC18/lessons/L6/clipping-raster.html

    Reads raster, crops according to polygon and resamples

    Inputs:
        filepath (str)
        polygon (shapely.Polygon object)
    Returns:
        2D numpy array
    '''

    #### Load Raster
    r_data = rasterio.open(filepath)

    #### Crop

    # Coordinates in format for rasterio
    shapes = getFeatures(polygon) 

    # Crop
    from rasterio.mask import mask
    out_img, out_transform = mask(r_data, shapes=shapes, crop=True)
    
    # Update Metadata
    # TODO: may not need as treat as numpy, not back to raster?
    #out_meta = r_data.meta.copy()
    #out_meta.update({"driver": "GTiff",
    #                "height": out_img.shape[1],
    #                "width": out_img.shape[2],
    #                "transform": out_transform})

    #### 3D to 2D
    out_img = out_img[0]    

    #### Remove Outer Border [lines of zeros] #TODO: See why happening?
    out_img = np.delete(out_img,  1, axis=0)
    out_img = np.delete(out_img, -1, axis=0)
    out_img = np.delete(out_img,  1, axis=1)
    out_img = np.delete(out_img, -1, axis=1)

    #### Resample
    out_img = cv2.resize(out_img, dsize=(img_height, img_width), interpolation=cv2.INTER_NEAREST)

    return out_img

#def resample(new_height, new_width):
#    '''
#    Rescale masked images to new_width and new_height.
#
#    Inputs:
#        new_width, new_height (int)
#    Returns:
#        data (numpy.ndarray)
#    '''
#    with rasterio.open('masked.tif') as dataset:
#        # Resampling.nearest
#        data = dataset.read(out_shape=(dataset.count, new_height, new_width),
#                            resampling=Resampling.bilinear)
#    
#    return data

    
def get_DTL(row, directory, bands, img_height, img_width, year):
    '''
    For a given VIIRS observation, grab and crop corresponding DLT data.
    
    Inputs:
        row (pandas.Series)
        directory (str) 
    Returns: 
        all_bands: (list) list of 7 arrays, each array is 3D
    '''
    all_bands = []

    for b in bands:

        tile, polygon = int(row['tile_id']), row['geometry']
        filename = f'l8_2014_tile{str(tile)}_b{str(b)}.tif'
        filepath = os.path.join(directory, filename)

        data = read_crop_resample_raster(filepath, polygon, img_height, img_width)

        all_bands.append(data)

        #if data.shape != (img_height, img_width):
        #    print(f'Flag: Irregular cropped image shape {data.shape}')
            
    all_bands_array = np.stack(all_bands, axis=-1)

    return all_bands_array


def map_DTL_NTL(input_gdf, directory, bands, img_height, img_width, year):
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

    for i in range(gdf.shape[0]):

        # Print every 100
        if (i % 100) == 0: print(i)

        row = gdf.iloc[[i]]

        DTL = get_DTL(row, directory, bands, img_height, img_width, year)
        if DTL.shape[0] == img_height:
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
    