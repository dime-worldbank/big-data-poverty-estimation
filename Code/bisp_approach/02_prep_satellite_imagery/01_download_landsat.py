# https://geoscripting-wur.github.io/Earth_Engine/

import ee
import geopandas as gpd
import webbrowser
import glob
import os
import time
import zipfile
import re
import geetools
import itertools
ee.Initialize()

# Setup -----------------------------------------------------------------------
# Filepaths
project_file_path = '/Users/robmarty/Dropbox/World Bank/IEs/Pakistan Poverty Estimation from Satellites/'
data_directory = project_file_path + 'Data/RawData/Landsat/unioncouncil/unstacked/'

# Parameters
uc_id = 'uc_1'
begin_date = '2016-01-01'
end_date = '2016-12-31'
cloud_cover_filter = 15
resolution = 30

pak_gpd = gpd.read_file(project_file_path + 'Data/FinalData/UC with NSER Data/uc_nser.geojson')

# Define Useful Functions ------------------------------------------------------
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

# Loop Through Union Councils; Extract/Download Imagery ------------------------
for uc_id in pak_gpd['uc_id'][764:2000]:

    # Pakistan Union Council i
    pakistan = ee.FeatureCollection('users/robmarty3/uc_nser')
    pakistan_i = pakistan.filter(ee.Filter.eq('uc_id', uc_id))

    # Load and Filter Images By Region, Date and Cloud Cover
    image = ee.ImageCollection('LANDSAT/LE07/C01/T1_SR')
    image = image.filterBounds(pakistan_i)
    image = image.filterDate(begin_date, end_date)
    image = image.filterBounds(pakistan_i)
    image = image.filter(ee.Filter.lt('CLOUD_COVER', cloud_cover_filter))

    # Filter by Cloud Cover
    image = image.map(geetools.cloud_mask.landsat457SR_pixelQA())

    # Collapse Images
    image = image.reduce(ee.Reducer.median())

    # Make Bands Same Type, and Integer
    image = image.multiply(1000).round().int()

    # Coordinates embedded differently: extract
    try:
        pakistan_i_coordinates = pakistan_i.geometry().getInfo()['coordinates']
    except:
        pakistan_i_coordinates_list = pakistan_i.geometry().getInfo()['geometries']
        for i in range(len(pakistan_i_coordinates_list)):
            if pakistan_i_coordinates_list[i]['type'] == 'Polygon':
                polygon_i = i
        pakistan_i_coordinates = pakistan_i.geometry().getInfo()['geometries'][polygon_i]['coordinates']

    if(len(pakistan_i_coordinates) > 1): pakistan_i_coordinates = list(itertools.chain(*pakistan_i_coordinates))

    # Export Path
    image = image.clip(pakistan_i)
    path = image.getDownloadUrl({
        'scale': resolution,
        'crs': 'EPSG:4326',
        'region': pakistan_i_coordinates
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

    if os.path.isdir(data_directory + uc_id) == False: os.mkdir(data_directory + uc_id)

    # Extract from ZipFile in Relevant Folder
    dir_files_initial = glob.glob(data_directory + uc_id + "/*")
    zip_ref = zipfile.ZipFile(zip_file[0], 'r')
    zip_ref.extractall(data_directory + uc_id)
    zip_ref.close()

    # Delete tpw files
    files_in_directory = os.listdir(data_directory + uc_id)
    for item in files_in_directory:
        if item.endswith(".tfw"):
            os.remove(os.path.join(data_directory + uc_id, item))

    # Delete Uncessary Bands
    files_in_directory = os.listdir(data_directory + uc_id)
    for item in files_in_directory:
        if item.endswith("mask_median.tif"):
            os.remove(os.path.join(data_directory + uc_id, item))
        if item.endswith("pixel_qa_median.tif"):
            os.remove(os.path.join(data_directory + uc_id, item))
        if item.endswith("radsat_qa_median.tif"):
            os.remove(os.path.join(data_directory + uc_id, item))
        if item.endswith("sr_atmos_opacity_median.tif"):
            os.remove(os.path.join(data_directory + uc_id, item))
        if item.endswith("sr_cloud_qa_median.tif"):
            os.remove(os.path.join(data_directory + uc_id, item))

    # Rename tif files
    files_in_directory = os.listdir(data_directory + uc_id)
    for orig_name in files_in_directory:
        new_name = 'B'+ re.sub(".*B","",orig_name)
        os.rename(data_directory + uc_id + "/" + orig_name, data_directory + uc_id + "/" + new_name)

    # Delete Zip File
    os.remove(zip_file[0])
    time.sleep(5)
