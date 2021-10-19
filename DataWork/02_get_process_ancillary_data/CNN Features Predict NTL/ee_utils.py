import ee
import numpy as np
import geetools
from geetools import ui, cloud_mask
import os, datetime
import pandas as pd
import itertools
import tensorflow as tf

cloud_mask_landsatSR = cloud_mask.landsatSR()
cloud_mask_sentinel2 = cloud_mask.sentinel2()

# tfrecord helper functions ----------------------------------------------------
# https://stackoverflow.com/questions/52324515/passing-multiple-inputs-to-keras-model-from-tf-dataset-api
# https://www.tensorflow.org/tutorials/load_data/tfrecord

def _bytes_feature(value):
    """Returns a bytes_list from a string / byte."""
    # If the value is an eager tensor BytesList won't unpack a string from an EagerTensor.
    if isinstance(value, type(tf.constant(0))):
        value = value.numpy() 
    return tf.train.Feature(bytes_list=tf.train.BytesList(value=[value]))

def _float_feature(value):
    """Returns a float_list from a float / double."""
    return tf.train.Feature(float_list=tf.train.FloatList(value=[value]))

def _int64_feature(value):
    """Returns an int64_list from a bool / enum / int / uint."""
    return tf.train.Feature(int64_list=tf.train.Int64List(value=[value]))

def chunk_ids(total_length, chunk_size):
    n_numbers = np.ceil(total_length / chunk_size)
    n_numbers = int(n_numbers)
    
    chunk_ids = list(range(0,n_numbers)) * chunk_size
    chunk_ids.sort()
    chunk_ids = chunk_ids[:total_length]
    
    return chunk_ids

# Main Functions -----------------------------------------------------------------
def survey_to_fc(survey_df):
    '''
    Convert pandas dataframe of survey locations to a feature collection. 
    
    Inputs:
        survey_df: pandas dataframe of survey locations. Function assumes 
                   the dataframe contains (1) latitude, (2) longitude and
                   (3) uid variables. Assumes coordinates in WGS84.
    Returns:
        (feature collection)
    '''
    
    survey_fc_list = []
    
    n_rows = survey_df.shape[0]
    for i in range(0, n_rows):
        survey_df_i = survey_df.iloc[[i]]

        f_i = ee.Feature(ee.Geometry.Point([survey_df_i['longitude'].iloc[0], 
                                            survey_df_i['latitude'].iloc[0]]), 
                         {'uid': survey_df_i['uid'].iloc[0]})

        survey_fc_list.append(f_i)
        
    survey_fc = ee.FeatureCollection(survey_fc_list)
    
    return survey_fc

def normalized_diff(values1, values2):
    '''
    Normalized Difference Value

    Input:  values1, values2 (must be same dimensions)

    Output: np array
    '''

    return (values2 - values1)/(values2 + values1)

def ee_to_np_daytime(daytime_f, survey_df, n_rows, b_b, g_b, r_b):
    '''
    Transforms feature collection from neighborhood array to np array. Stacks bands
    so that they are: NTL, blue, green, red, NDVI, other single daytime bands

    Input:  
      f (features)
      n_rows (number of features)

    Output: np array
    '''
    
    example_proto_list = []

    for i in range(0, n_rows):
        survey_uid = survey_df['uid'].iloc[i]
        #folder_name = survey_df['tf_folder_name'].iloc[i]
        viirs_ntl_group = int(survey_df['ntl_group'].iloc[i])
        survey_year_i = int(survey_df['year'].iloc[i])
        uid_i = survey_df['uid'].iloc[i].encode()
        
        d_f_i = daytime_f[i]['properties']
        #n_f_i = ntl_f[i]['properties']

        # SAVE AS TFRECORD

        # Prep Files
        ### RGB
        brgb_l = [np.array(d_f_i[r_b]), np.array(d_f_i[g_b]), np.array(d_f_i[b_b])]
        brgb_np = np.stack(brgb_l, axis=-1)
        brgb_np = brgb_np.astype(np.uint16)
        brgb_np_tf = tf.io.encode_png(brgb_np, compression = 9)
        #brgb_np_tf = tf.io.serialize_tensor(brgb_np)

        # https://www.tensorflow.org/api_docs/python/tf/io/encode_png
        ### NDVI 
        bndvi_np = d_f_i['NDVI']      
        bndvi_np = np.expand_dims(bndvi_np, axis=2) # original (224, 224), change to (224,224,1) -> so can stack
        # Convert from -1 to 1 to 0 to 20000
        bndvi_np = bndvi_np + 1
        bndvi_np = bndvi_np * 10000
        bndvi_np = bndvi_np.astype(np.uint16)
        bndvi_np_tf = tf.io.encode_png(bndvi_np, compression = 9)
        #bndvi_np_tf = tf.io.serialize_tensor(bndvi_np)

        ### BU 
        bbu_np = d_f_i['BU']      
        bbu_np = np.expand_dims(bbu_np, axis=2) # original (224, 224), change to (224,224,1) -> so can stack
        # Convert from -1 to 1 to 0 to 20000
        bbu_np = bbu_np + 1
        bbu_np = bbu_np * 10000
        bbu_np = bbu_np.astype(np.uint16)
        bbu_np_tf = tf.io.encode_png(bbu_np, compression = 9)
        #bndvi_np_tf = tf.io.serialize_tensor(bndvi_np)

        ### NTL
        # Not uint16, so so serialize
        #bntl_np = np.array(n_f_i['avg_rad'])
        #bntl_np = np.expand_dims(bntl_np, axis=2)
        # Values to uint16
        #bntl_np = bntl_np + 2 # Can be negative
        #bntl_np = bntl_np * 100 # consider two decimal places before uint16 // could also to * 10 (second decimal may not matter)
        #bntl_np[bntl_np >= 65535] = 65535 # within range of uint16
        #bntl_np = bntl_np.astype(np.uint16)
        #bntl_np_tf = tf.io.encode_png(bntl_np, compression = 9)
        #bntl_np_tf = tf.io.serialize_tensor(bntl_np)

        ## Create dictionary
        feature = {
            'uid' : _bytes_feature(uid_i),
            'viirs_ntl_group' : _int64_feature(viirs_ntl_group),
            'year' : _int64_feature(survey_year_i),
            'b_rgb': _bytes_feature(brgb_np_tf),
            'b_ndvi': _bytes_feature(bndvi_np_tf),
            'b_bu': _bytes_feature(bbu_np_tf)
            }

        # Other MS Bands
        #b_other_list = []
        #for b_other_i in other_bs:
        #    bi_np = np.array(d_f_i[b_other_i])
        #    bi_np = np.expand_dims(bi_np, axis=2)
        #    #bi_np_tf = tf.io.serialize_tensor(bi_np)
        #    bi_np = bi_np.astype(np.uint16)
        #    bi_np_tf = tf.io.encode_png(bi_np, compression = 9)
        #    feature['b_' + b_other_i] = _bytes_feature(bi_np_tf)
  
        example_proto = tf.train.Example(features=tf.train.Features(feature=feature))

        example_proto_list.append(example_proto)

        #out_file_name = os.path.join(out_path, folder_name, survey_uid + '.tfrecord')
        #with tf.io.TFRecordWriter(out_file_name) as writer:
        #  writer.write(example_proto.SerializeToString())
        
    return example_proto_list

        #bndvi_np = np.expand_dims(bndvi_l, axis=2)
        #b_np = np.expand_dims(b_l, axis=2)
        #b_np = np.repeat(b_np, 3, -1)
        #np.save(os.path.join(out_path, band_i + "_" + survey_uid + '.npy'), b_np)
        #np.save(os.path.join(out_path, 'BRGB' + "_" + survey_uid + '.npy'), brgb_np)
        #bndvi_np = np.repeat(bndvi_np, 3, -1)
        #np.save(os.path.join(out_path, 'BNDVI' + "_" + survey_uid + '.npy'), bndvi_np)

        #for band_i in SINGLE_BANDS_ALL:
        #    
        #    b_l = np.array(f_i[band_i])
        #    b_np = np.expand_dims(b_l, axis=2)
        #    #b_np = np.repeat(b_np, 3, -1)
        #    np.save(os.path.join(out_path, band_i + "_" + survey_uid + '.npy'), b_np)

    
    #return "Done"

def prep_cnn_np(survey_df,
                satellite,
                kernel_size,
                year):
    '''
    Creates numpy arrays for CNN

    Input:  df - pandas dataframe
            lat_name - name of latitude variable in df
            lon_name - name of longitude variable in df
    Output: geopandas dataframe
    '''

    # Setup --------------------------------------------------------------------
    # Survey to FeatureCollection  
    survey_fc = survey_to_fc(survey_df)

    # Define kernel for neighborhood array
    list = ee.List.repeat(1, kernel_size)
    lists = ee.List.repeat(list, kernel_size)
    kernel = ee.Kernel.fixed(kernel_size, kernel_size, lists)

    # Define scale
    if satellite in ['l7', 'l8']:
        SCALE = 30
    elif satellite in ['s2']: 
        SCALE = 10

    # Prep NTL -----------------------------------------------------------------
    
    # Year
    # VIIRS starts in 2012. At minimum, use 2013 to have year before and after
    if False:
        if year <= 2013:
            year_use = 2013
        else:
            year_use = year

        year_plus = year_use + 1
        year_minus = year_use - 1

        year_minus_str = str(year_minus) + '-01-01'
        year_plus_str = str(year_plus) + '-12-31'

        # Reduce image collection
        ntl_image = ee.ImageCollection('NOAA/VIIRS/DNB/MONTHLY_V1/VCMCFG')\
            .filterDate(year_minus_str, year_plus_str)\
            .median()

        # Select Bands  
        ntl_image = ntl_image.select(['avg_rad'])

        # Image to neighborhood array
        ntl_arrays = ntl_image.neighborhoodToArray(kernel)

        # Extract values from GEE    
        ntl_values_ee = ntl_arrays.sample(
          region = survey_fc, 
          scale = SCALE,
          tileScale = 8
        )

        ntl_dict_ee = ntl_values_ee.getInfo()

        # Convert values to numpy array
        #n_rows = survey_df.shape[0]
        ntl_f = ntl_dict_ee['features']    
        
    # l7 ----------------------------------------------------------------
    if satellite == "l7":
        
        # Bands
        b_b = 'B1'
        g_b = 'B2' 
        r_b = 'B3' 
        nir_b = 'B4'
        swir_b = 'B5'
        #other_bs = ['B5', 'B6', 'B7']
        
        #BANDS = single_bs.copy()
        BANDS = [b_b].copy()
        BANDS.append(g_b)
        BANDS.append(r_b)
        BANDS.append(nir_b)
        BANDS.append(swir_b)

        # Year
        year_use = year
        
        year_plus = year_use + 1
        year_minus = year_use - 1
        
        year_minus_str = str(year_minus) + '-01-01'
        year_plus_str = str(year_plus) + '-12-31'
        
        image = ee.ImageCollection('LANDSAT/LC07/C01/T1_SR')\
            .filterDate(year_minus_str, year_plus_str)\
            .map(cloud_mask_landsatSR)\
            .median() #\
            #.multiply(0.0001)
    
    # l8 ----------------------------------------------------------------
    if satellite == "l8":
                
        # Bands
        # FOR COLLECTION 2
        #b_b = 'SR_B2'
        #g_b = 'SR_B3' 
        #r_b = 'SR_B4' 
        #nir_b = 'SR_B5'
        #other_bs = ['SR_B6', 'SR_B7', 'ST_B10']
        
        # FOR COLLECTION 1
        b_b = 'B2'
        g_b = 'B3' 
        r_b = 'B4' 
        nir_b = 'B5'
        swir_b = 'B6'
        #other_bs = ['B6', 'B7', 'B10']
        
        #BANDS = single_bs.copy()
        BANDS = [b_b].copy()
        BANDS.append(g_b)
        BANDS.append(r_b)
        BANDS.append(nir_b)
        BANDS.append(swir_b)

        # Year
        # landsat 8 starts in April 2013; if year is less than
        # 2014, use 2014 as year (to ensure have year before and after)
        if year < 2014:
            year_use = 2014
        else:
            year_use = year
                    
        year_plus = year_use + 1
        year_minus = year_use - 1
        
        year_minus_str = str(year_minus) + '-01-01'
        year_plus_str = str(year_plus) + '-12-31'
        
        #image = ee.ImageCollection('LANDSAT/LC08/C02/T1_L2')\
        #    .filterDate(year_minus_str, year_plus_str)\
        #    #.map(cloud_mask_landsatSR)\ #TODO cloud_mask_landsatSR doesn't work with landsat collection 2
        #    .median() #\
        #    #.multiply(0.0001)
        
        image = ee.ImageCollection('LANDSAT/LC08/C01/T1_SR')\
            .filterDate(year_minus_str, year_plus_str)\
            .map(cloud_mask_landsatSR)\
            .median()
        
        #image = ee.ImageCollection('LANDSAT/LC08/C02/T1_L2')\
        #    .filterDate(year_minus_str, year_plus_str)\
        #    .median()
            
    # s2 ----------------------------------------------------------------
    if satellite == "s2":
        
        # Bands
        b_b = 'B2'
        g_b = 'B3' 
        r_b = 'B4' 
        nir_b = 'B8'
        swir_b = 'B11'
        #other_bs = ['B5', 'B6', 'B7', 'B8A', 'B11', 'B12', 'AOT']
     
        #BANDS = single_bs.copy()
        BANDS = [b_b].copy()
        BANDS.append(g_b)
        BANDS.append(r_b)
        BANDS.append(nir_b)
        BANDS.append(swir_b)
        
        # Year
        # sentinel starts in March 2017; juse use 2018
        year_use = 2019
                    
        year_plus = year_use + 1
        year_minus = year_use - 1
        
        year_minus_str = str(year_minus) + '-01-01'
        year_plus_str = str(year_plus) + '-12-31'

        # Number of bands changes in sentinel, so need to select here before aggregate
        # https://gis.stackexchange.com/questions/374010/gee-tile-error-expected-a-homogeneous-image-collection-but-an-image-with-incom
        image = ee.ImageCollection('COPERNICUS/S2_SR')\
            .filterDate(year_minus_str, year_plus_str)\
            .map(cloud_mask_sentinel2)\
            .select(BANDS)\
            .median() # \
            #.multiply(0.0001)

    # Select Bands
    image = image.select(BANDS)
    
    # Create Indices
    # https://www.linkedin.com/pulse/ndvi-ndbi-ndwi-calculation-using-landsat-7-8-tek-bahadur-kshetri
    ndvi = image.normalizedDifference([nir_b, r_b]).rename('NDVI');
    ndbi = image.normalizedDifference([swir_b, nir_b]).rename('NDBI');
    image = image.addBands(ndvi)
    image = image.addBands(ndbi)
        
    bu = image.select('NDBI').subtract(image.select('NDVI')).rename('BU')
    image = image.addBands(bu)
        
    # Image to neighborhood array
    arrays = image.neighborhoodToArray(kernel)

    # Extract values from GEE   
    values_ee = arrays.sample(
      region = survey_fc, 
      scale = SCALE,
      tileScale = 8
    )
    
    dict_ee = values_ee.getInfo()
    
    # Convert values to numpy array
    n_rows = survey_df.shape[0]
    daytime_f = dict_ee['features']
    
    # Extract dta
    out_ex_proto_list = ee_to_np_daytime(daytime_f, survey_df, n_rows, b_b, g_b, r_b)
    
    return out_ex_proto_list

