# Python 3
#### MOVE TO MAIN FOLDER

# https://geoscripting-wur.github.io/Earth_Engine/

import ee
import pandas as pd
import geopandas as gpd
import numpy as np
import webbrowser
import glob
import os
import time
import zipfile
import re
import geetools
import itertools
import math
ee.Initialize()

# Setup -----------------------------------------------------------------------
# Filepaths
project_file_path = '/Users/robmarty/Dropbox/World Bank/IEs/Pakistan Poverty Estimation from Satellites/'
data_directory = project_file_path + 'Data/RawData/Landsat/bisp_households/2014/unstacked/'

bisp_coordinates_filepath = '/Users/robmarty/Desktop/'

# Parameters
buffer_radius = 1.5/111.12
begin_date = '2014-01-01'
end_date = '2014-12-31'
cloud_cover_filter = 15
resolution = 30

# Define Useful Functions ------------------------------------------------------
def get_lat_lon(number):
  deg = math.floor(number / 100)
  min = math.floor(number - (100 * deg))
  sec = 100 * (number - (100 * deg) - min)
  degree = deg + (min / 60) + (sec / 3600)

  return(degree)
get_lat_lon_vec = np.vectorize(get_lat_lon)

# https://stackoverflow.com/questions/35851281/python-finding-the-users-downloads-folder
def get_download_path():
    """Returns the default downloads path for linux or windows"""
    if os.name == 'nt':
        import winreg
        sub_key = r'SOFTWARE\Microsoft\Windows\CurrentVersion\Explorer\Shell Folders'
        downloads_guid = '{374DE290-123F-4565-9164-39C4925E467B}'
        with winreg.OpenKey(winreg.HKEY_CURRENT_USER, sub_key) as key:
            location = winreg.QueryValueEx(key, downloads_guid)[0]
        return location
    else:
        return os.path.join(os.path.expanduser('~'), 'downloads')

# Load and Prep BISP HH Coordinates ---------------------------------------------
##### Individual
#bisp_coords_df = pd.read_stata(bisp_coordinates_filepath + 'GPS_uid_crosswalk.dta')
#bisp_coords_df['id'] = np.arange(bisp_coords_df.shape[0]) + 1
#bisp_coords_df = bisp_coords_df.dropna()

#bisp_coords_df['lat'] = get_lat_lon_vec(bisp_coords_df['GPSN'])
#bisp_coords_df['lon'] = get_lat_lon_vec(bisp_coords_df['GPSE'])

# bisp_coords_df['max_dist'] = 0

##### Clusters
bisp_coords_df = pd.read_csv(project_file_path + 'Data/RawData/Landsat/bisp_households/bisp_cluster_coords.csv')
bisp_coords_df['lat'] = bisp_coords_df['latitude']
bisp_coords_df['lon'] = bisp_coords_df['longitude']
bisp_coords_df['id'] = bisp_coords_df['cluster_id']

#bisp_coords_df['id'][1078:2000]
#bisp_coords_df['id'][110:2000]
# Loop Through Households; Extract/Download Imagery ------------------------
# bisp_coords_df['id']
#bisp_coords_df['id'][100:2000]
#bisp_coords_df['id'][420:2000]
#bisp_coords_df['id'][1487:2000]
# 4990
#bisp_coords_df['id']
for hh_id in bisp_coords_df['id'][1640:2000]:

    print(hh_id)

    bisp_coords_df_i = bisp_coords_df[bisp_coords_df.id == hh_id]

    buffer_radius_i = float(bisp_coords_df_i['dist_max']) + buffer_radius

    hh_buffer = ee.Geometry.Rectangle([float(bisp_coords_df_i['lon'] - buffer_radius_i),
                                  float(bisp_coords_df_i['lat'] - buffer_radius_i),
                                  float(bisp_coords_df_i['lon'] + buffer_radius_i),
                                  float(bisp_coords_df_i['lat'] + buffer_radius_i)])

    # Load and Filter Images By Region, Date and Cloud Cover
    image = ee.ImageCollection('LANDSAT/LE07/C01/T1_SR')
    image = image.filterBounds(hh_buffer)
    image = image.filterDate(begin_date, end_date)
    #image = image.filterBounds(pakistan_i)
    image = image.filter(ee.Filter.lt('CLOUD_COVER', cloud_cover_filter))

    # Filter by Cloud Cover
    image = image.map(geetools.cloud_mask.landsat457SR_pixelQA())

    # Collapse Images
    image = image.reduce(ee.Reducer.median())

    # Make Bands Same Type, and Integer
    #image = image.multiply(1000).round().int()

    # Grab Coordinates
    hh_buffer_coordinates = hh_buffer.getInfo()['coordinates']

    # Export Path
    image = image.clip(hh_buffer)
    path = image.getDownloadUrl({
        'scale': resolution,
        'crs': 'EPSG:4326',
        'region': hh_buffer_coordinates
    })

    # Initial Files List
    files_initial = glob.glob(get_download_path() + "/*.zip")
    length_files_initial = len(files_initial)

    # Download file and wait until downloads
    implement_download = False
    while(len(glob.glob(get_download_path() + "/*.zip")) == len(files_initial)):
        if implement_download == False:
            webbrowser.open(path)
            implement_download = True

        #print("Waiting for File to Download ...")
        time.sleep(5)

    # Grab zip file name
    files_after = glob.glob(get_download_path() + "/*.zip")
    zip_file = list(set(files_after) - set(files_initial))

    hh_id = str(hh_id)
    if os.path.isdir(data_directory + hh_id) == False: os.mkdir(data_directory + hh_id)

    # Extract from ZipFile in Relevant Folder
    dir_files_initial = glob.glob(data_directory + hh_id + "/*")
    zip_ref = zipfile.ZipFile(zip_file[0], 'r')
    zip_ref.extractall(data_directory + hh_id)
    zip_ref.close()

    # Delete tpw files
    files_in_directory = os.listdir(data_directory + hh_id)
    for item in files_in_directory:
        if item.endswith(".tfw"):
            os.remove(os.path.join(data_directory + hh_id, item))

    # Delete Uncessary Bands
    files_in_directory = os.listdir(data_directory + hh_id)
    for item in files_in_directory:
        if item.endswith("mask_median.tif"):
            os.remove(os.path.join(data_directory + hh_id, item))
        if item.endswith("pixel_qa_median.tif"):
            os.remove(os.path.join(data_directory + hh_id, item))
        if item.endswith("radsat_qa_median.tif"):
            os.remove(os.path.join(data_directory + hh_id, item))
        if item.endswith("sr_atmos_opacity_median.tif"):
            os.remove(os.path.join(data_directory + hh_id, item))
        if item.endswith("sr_cloud_qa_median.tif"):
            os.remove(os.path.join(data_directory + hh_id, item))

    # Rename tif files
    files_in_directory = os.listdir(data_directory + hh_id)
    for orig_name in files_in_directory:
        new_name = 'B'+ re.sub(".*B","",orig_name)
        os.rename(data_directory + hh_id + "/" + orig_name, data_directory + hh_id + "/" + new_name)

    # Delete Zip File
    os.remove(zip_file[0])
    time.sleep(0.2)
