# Get and Process Ancillary Data

Cleans individual datasets and, when relevant, extracts values to OPM data (e.g., average nighttime light locations around survey locations).

* __Country Grid:__ Create a grid that covers Pakistan. Landsat satellite images are extracted for each grid. (This grid is uploaded to Google Earth Engine for processing).
* __Facebook:__ Scrape and clean Facebook marketing data (daily and monthly active users for different user types). Scrapes information around OPM data points
* __GADM:__ Download GADM administrative polygons
* __Landsat:__ Code for extracting/downloading data is saved in Google Earth Engine. Code extracts Landsat data for each grid in `Country Grid`
* __OSM:__ Extracts OpenStreetMap information to OPM survey locations
* __VIIRS:__ (a) Converts VIIRS grid into polygon, which is needed later to grab landsat grid cells within polygon, and (b) extracts VIIRS values to OPM survey locations



