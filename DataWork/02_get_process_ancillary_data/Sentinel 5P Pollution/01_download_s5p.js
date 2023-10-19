// ** NOTE: To run in Goolge Earth Engine Code Editor **

// =============================================================
// Extract VIIRS to Country
// =============================================================

// =============================================================
  // Function to Extract VIIRS Annual Average Data
var extract_viirs = function(dataset,
                             band,
                             google_drive_folder){
  // Extracts and summarizes VIIRS data
  
  // Args:
    //   begin_date: Beginning data for VIIRS image.
  //   end_date: End date for VIIRS image.
  //   country_name: Country to export
  //   reduce_type: How to reduce/summarize data ("median","mean" or "max")
  //   file_name: Name to call exported image
  //   google_drive_folder: Name of folder to export image
  
  // Get country feature
  var country = ee.FeatureCollection('USDOS/LSIB_SIMPLE/2017');
  //var country = countries.filter(ee.Filter.eq('country_co', country_name));
  
  // Load VIIRS ImageCollection and filter by date
  
  
  if(dataset == "uv_aer"){
    var image = ee.ImageCollection("COPERNICUS/S5P/OFFL/L3_AER_AI")
  }
  
  if(dataset == "CO"){
    var image = ee.ImageCollection("COPERNICUS/S5P/OFFL/L3_CO")
  }
  
  if(dataset == "HCHO"){
    var image = ee.ImageCollection("COPERNICUS/S5P/OFFL/L3_HCHO")
  }
  
  if(dataset == "ozone"){
    var image = ee.ImageCollection("COPERNICUS/S5P/OFFL/L3_O3")
  }
  
  if(dataset == "SO2"){
    var image = ee.ImageCollection("COPERNICUS/S5P/OFFL/L3_SO2")
  }
  
  if(dataset == "CH4"){
    var image = ee.ImageCollection("COPERNICUS/S5P/OFFL/L3_CH4")
  }
  
  if(dataset == "NO2"){
    var image = ee.ImageCollection("COPERNICUS/S5P/OFFL/L3_NO2")
  }
  
  image = image.filterDate('2019-01-01', '2021-12-31');
  image = image.reduce(ee.Reducer.mean()); 
  image = image.select(band + '_mean');
  
  // Clip to country and export image to google drive
  // image = image.clip(country);
  
  Export.image.toDrive({
    folder: google_drive_folder,
    image: image,
    scale: 3500,
    // region: country,
    description: band + "_3_5km",
    maxPixels: 999999999
  });
  
  return image;
  
};

// =============================================================
// Export VIIRS Data to Google Drive

var data = extract_viirs('uv_aer', 'absorbing_aerosol_index', 'gee_extracts');

var data = extract_viirs('CO', 'CO_column_number_density', 'gee_extracts');
var data = extract_viirs('CO', 'H2O_column_number_density', 'gee_extracts');

var data = extract_viirs('HCHO', 'tropospheric_HCHO_column_number_density', 'gee_extracts');
var data = extract_viirs('HCHO', 'tropospheric_HCHO_column_number_density_amf', 'gee_extracts');

var data = extract_viirs('NO2', 'NO2_column_number_density', 'gee_extracts');
var data = extract_viirs('NO2', 'tropospheric_NO2_column_number_density', 'gee_extracts');
var data = extract_viirs('NO2', 'stratospheric_NO2_column_number_density', 'gee_extracts');
var data = extract_viirs('NO2', 'NO2_slant_column_number_density', 'gee_extracts');

var data = extract_viirs('ozone', 'O3_column_number_density', 'gee_extracts');
var data = extract_viirs('ozone', 'O3_effective_temperature', 'gee_extracts');

var data = extract_viirs('SO2', 'SO2_column_number_density', 'gee_extracts');
var data = extract_viirs('SO2', 'SO2_column_number_density_amf', 'gee_extracts');
var data = extract_viirs('SO2', 'SO2_slant_column_number_density', 'gee_extracts');

var data = extract_viirs('CH4', 'CH4_column_volume_mixing_ratio_dry_air', 'gee_extracts');

  