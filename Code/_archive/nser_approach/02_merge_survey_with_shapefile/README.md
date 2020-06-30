# Geolocate NSER Survey

The NSER survey provides the name of the district, tehsil and union council that each observation is in. We implement multiple techniques to geolocate the names: (1) matching NSER survey names to district/tehsil/UC names from existing union-council level shapefiles and (2) running the names through geocoders, such as the Google API geocoder.

### Code Description
* 01_clean_nser_survey.R: Cleans NSER Survey. Generates a shapefile with just ID and ADM names, and another shapefile with all NSER data.
* 02_merge_survey_with_hdx_pak_boundary.R: Matches NSER survey names with ADM names from ``HDX'' Pakistan Union-Council Level Shapefile (https://data.humdata.org/dataset/pakistan-union-council-boundaries-along-with-other-admin-boundaries-dataset)
* 03a_unioncouncil_google_geocoding_api.R: Runs NSER survey names into Google API geocoder and extracts coordinates
* 03b_google_geocoding_determine_correct_coords.R: Determines which Google Coordinates to accept as correct.
* 04_create_final_geolocations.R: Using all geolocation techniuqes, develops a final, geolocated NSER survey


