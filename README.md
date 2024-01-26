# Replication Package for: Global poverty estimation using private and public sector big data sources

This repository contains code to replicate analysis and includes a link to download processed, analysis-ready datasets. The [`_main.R`](https://github.com/dime-worldbank/big-data-poverty-estimation/blob/master/_main.R) script runs or points to all code that needs to be run for the analysis. `_main.R` is initially set up so that it loads the analysis-ready datasets, runs analysis, and creates all figures and tables; it skips creating the analysis-ready datasets from the raw data, which is a more cumbersome process.

However, some users may be interested in re-creating the analysis-ready datasets. By changing a parameter, `_main.R` will also run scripts to process raw data to create the analysis-ready datasets. However, (1) raw data from a number of sources must be manually downloaded and (2) a few scripts are run in Stata and Python; `_main.R` points to these scripts, but they must be manually opened and run.

This replication package is divided into two parts:

1. [Create Analysis-Ready Datasets from Raw Data](#replicate-data). Contains instructions for manually downloading raw data, and steps for running code. While the `_main.R` script helps to automate running relevant scripts, code from other software (Stata and Python) must be manually run.
2. [Replicating analysis, starting from analysis-ready datasets](#replicate-analysis). Analysis-ready datasets are provided, and the `_main.R` script can be set to easily run all analysis code.

## Create Analysis-Ready Datasets from Raw Data <a name="replicate-data"></a>

### Download raw data

Download data directory from [here](https://www.dropbox.com/scl/fo/1luod4j82jbyooejaaorm/h?rlkey=zohapjaph445bdya9ipwaeve2&dl=0). This directory contains the analysis-ready data files, as well as folders where raw data must be placed. The `Data` folder contains sub-folders for each dataset. The sub-folders generally contain a `RawData` folder for raw data and a `FinalData` folder for data processed by code. In many cases, code is used to automatically download data. However, the following datasets need to be manually downloaded:

1. __Global DHS Data:__ Download data from the [DHS website](https://dhsprogram.com/data/) to be put in `Data/DHS/RawData`; this directory contains folders that indicate which datasets need to be downloaded. For example, 2020 data for Kenya for the "HR" (Household Recode) dataset should be placed here: `/KE/KE_2020_MIS_03292022_2054_82518/KEHR81DT`.
2. __Nigeria DHS Data:__ The paper includes specific analysis for Nigeria. Following a similar process as above, data should be placed in `Data/DHS_nga_policy_experiment/RawData`.
3. __LSMS Data:__ Download LSMS data from the [World Bank Microdata catalogue](https://microdata.worldbank.org/index.php/catalog/lsms/?page=1&ncsrf=ea3643ebcba3745efa4469f5e827e107&ps=15&repo=lsms). `Data/LSMS/RawData/individual_files` includes a folder for each country. Within each country folder, there is a `README Files to Download.md` file which lists the individual datasets that need to be downloaded into the folder. 
4. __Harmonized Nighttime Lights:__ Download data from [here](https://figshare.com/articles/dataset/Harmonization_of_DMSP_and_VIIRS_nighttime_light_data_from_1992-2018_at_the_global_scale/9828827/5) and place in `Data/DMSPOLS_VIIRS_Harmonized/RawData`.
5. __ESA Land Cover Data:__ Download data from [here](https://cds.climate.copernicus.eu/cdsapp#!/dataset/satellite-land-cover?tab=form) and place in `Data/Globcover/RawData`.
  * For 1992 to 2015 data, put the `ESACCI-LC-L4-LCCS-Map-300m-P1Y-1992_2015-v2.0.7.tif` file in the `/1992_2015_data` folder
  * For 2016 to 2018 data, (1) put the `.nc` files in the `2016_2018` folder, then (2) use [this script](https://github.com/dime-worldbank/big-data-poverty-estimation/blob/master/DataWork/02_get_process_ancillary_data/Globcover/globcover_netcdf_to_geotiff.txt) to convert .nc files to .tif files.
  
6. __OpenStreetMap Data:__ Download data from [Geofabrik](https://download.geofabrik.de/). To find data for a specific country, (1) click the continent the country is in, (2) click the name of the country, (3) click "raw directory index", (4) and find the relevant date to download; the file that ends in `shp.zip` should be downloaded. Download the file and unzip it. Place the file in the relevant folder within `Data/OSM/RawData`; this folder contains subfolders for each country and year where OpenStreetMap data needs to be downloaded. For example, the data downloaded and unzipped from `kenya-210101-free.shp.zip` should be placed in `Data/OSM/RawData/kenya-210101-free.shp`

7. __Sentinel 5P Pollution Data:__ Run [this code](https://github.com/dime-worldbank/big-data-poverty-estimation/blob/master/DataWork/02_get_process_ancillary_data/Sentinel%205P%20Pollution/01_download_s5p.js) in the Google Earth Engine code editor, and put the data in `Data/Sentinel 5P Pollution/RawData`

### Setup and run code

1. Clone this repo
2. In [`_main.R`](https://github.com/dime-worldbank/big-data-poverty-estimation/blob/master/_main.R), change `dropbox_dir` to point to the data folder and `github_dir` to point to the github repo.
3. Create a folder in Google Drive, mount Google Drive to your computer, and change `gdrive_dir` to point to this folder. Code to download satellite imagery from Google Earth Engine requires a Google Drive folder; data from GEE is exported to Google Drive.
4. In [`_main.R`](https://github.com/dime-worldbank/big-data-poverty-estimation/blob/master/_main.R), ensure that `RUN_DATA_CREATION_CODE` is set to `TRUE` and `RUN_ANALYSIS_CODE` is set to `FALSE`. When `RUN_DATA_CREATION_CODE` is `TRUE`, code from the following sub-folders are run:

  * `00_download_gadm`: Downloads [GADM](https://gadm.org/) data that is used in cleaning survey data.
  * `01_clean_dhs`: Cleans [DHS](https://www.usaid.gov/global-health/demographic-and-health-surveys-program) survey data.
  * `01_clean_dhs_nga_experiment`: Cleans DHS survey data for Nigeria, using four rounds of data (used for __Application: Estimating Wealth in Different Years__ section of paper)
  * `01_clean_lsms`: Cleans [LSMS](https://www.worldbank.org/en/programs/lsms) survey data.
  * `02_get_process_ancillary_data`: Extracts and process data around survey locations, such as from satellites, OpenStreetMaps, and Facebook Marketing data.
  * `03_merge_ancillary_data_with_survey`: Merges ancillary data (satellite, OSM, Facebook data) extracted in previous step to survey data; creates cleaned, analysis-ready datasets.
  
5. Run the code. Running the [`_main.R`](https://github.com/dime-worldbank/big-data-poverty-estimation/blob/master/_main.R) script will run all R files. However, instead of running the `_main.R` script, we recommend running files one-by-one as scripts in Python and Stata need to be run as well; the `_main.R` script notes when these need to be run, but does not call these scripts (eg, indicating `*[RUN USING PYTHON]*`). Within Stata and Python scripts, follow directions for how these should be set up (eg, variables need to be changed to point to the data folder).
* The script to run convulational neural networks (`03_estimate_cnn_and_extract_features.ipynb`[https://github.com/dime-worldbank/big-data-poverty-estimation/blob/master/DataWork/02_get_process_ancillary_data/CNN%20Features%20Predict%20NTL/03_estimate_cnn_and_extract_features.ipynb]) was run by installing tensorflow designed to work with Apple's M1 chip (ie, Apple's hardware-accelerated TensorFlow) which significantly sped up the time to process the code. Instructions for setting up this environment can be found [here](https://github.com/dime-worldbank/big-data-poverty-estimation/blob/master/DataWork/02_get_process_ancillary_data/CNN%20Features%20Predict%20NTL/_INSTRUCTIONS_TO_INSTALL_TENSORFLOW_M1_MAC.txt).

6. Re-run the code when setting the `SURVEY_NAME` to (1) `DHS_nga_policy_experiment` and (2) `LSMS`. By default, the `SURVEY_NAME` parameter is set to `DHS`, to process data for `DHS` data. However, the `SURVEY_NAME` parameter (set in line 215) needs to be changed and the code re-run.


## Replicating analysis, starting from analysis-ready datasets <a name="replicate-analysis"></a>

### Steps

1. Clone this repository
2. Download data from [here](https://www.dropbox.com/scl/fo/1luod4j82jbyooejaaorm/h?rlkey=zohapjaph445bdya9ipwaeve2&dl=0). All data needed for code for this section is provided. The downloaded folder contains `data_readme.pdf`, which describes the datasets.
3. In [`_main.R`](https://github.com/dime-worldbank/big-data-poverty-estimation/blob/master/_main.R), change `dropbox_dir` to point to the data folder and `github_dir` to point to the github repo.
4. Run [`_main.R`](https://github.com/dime-worldbank/big-data-poverty-estimation/blob/master/_main.R). Ensure that `RUN_ANALYSIS_CODE` is set to `TRUE`. When `RUN_ANALYSIS_CODE` is set to `TRUE`, the `_main.R` script runs all code in the following sub-folders:

* `DataWork/04_poverty_estimation`: Implements machine learning models and appends results.
* `DataWork/05_figures_tables_global`: Produces figures and tables for paper. Figures and tables are exported to [`Paper Tables and Figures`](https://github.com/dime-worldbank/big-data-poverty-estimation/tree/master/Paper%20Tables%20and%20Figures); where [`Paper Tables and Figures/main.tex`](https://github.com/dime-worldbank/big-data-poverty-estimation/blob/master/Paper%20Tables%20and%20Figures/main.tex) compiles all the tables and figures for the main text together, and [`Paper Tables and Figures/supplementary_materials.tex`](https://github.com/dime-worldbank/big-data-poverty-estimation/blob/master/Paper%20Tables%20and%20Figures/supplementary_materials.tex) compiles all the tables and figures for the supplementary information/appendix document.

### Parameters in main script

[`_main.R`](https://github.com/dime-worldbank/big-data-poverty-estimation/blob/master/_main.R): Main script that runs all code for project.

At the beginning of the `_main.R` script, three parameters are set at the beginning.

1. `RUN_CODE`: If `TRUE`, runs other scripts (eg, creating figures and tables). If `FALSE`, just loads packages and sets filepaths.
2. `DELETE_ML_RESULTS`: A large number of machine learning models are implemented for the analysis (ie, a separate model for each country for each set of features, etc). After each model is implemented, results are exported (eg, predicted values from the model). [The script](https://github.com/dime-worldbank/big-data-poverty-estimation/blob/master/DataWork/04_poverty_estimation/01_pov_estimation.R) that implements the machine learning analysis checks which models have already been implemented by checking the results files. Only models that have not yet been implemented are implemented. Consequently, by default, the code will see all machine learning results and skip running machine learning models. By setting `DELETE_ML_RESULTS` to `TRUE`, machine learning results will be deleted, and machine learning models will be re-implemented. _NOTE:_ All machine learning models can take over 15 hours to run.
3. `EXPORT_TXT_REPORT_CODE_DURATION`: If set to `TRUE`, a .txt file will be exported that indicates how long the code took to run. 
The main script produces all figures and tables for the paper, with one minor exception; the main script does not produce the [figure with example daytime satellite images](https://github.com/dime-worldbank/big-data-poverty-estimation/blob/master/Paper%20Tables%20and%20Figures/figures/example_daytime_images.png). [This script](https://github.com/dime-worldbank/big-data-poverty-estimation/blob/33cbed1be65afcc50b373f88c0835df8078bac22/DataWork/02_get_process_ancillary_data/CNN%20Features%20Predict%20NTL/example_images.ipynb#L599) produces the figure, but the figure requires satellite data to be downloaded, which can be done using [this script](https://github.com/dime-worldbank/big-data-poverty-estimation/blob/33cbed1be65afcc50b373f88c0835df8078bac22/DataWork/02_get_process_ancillary_data/CNN%20Features%20Predict%20NTL/01_create_ntlgroup_tfrecord_name_ntlharmon.R). All other figures and tables are generated based on the cleaned datasets and subsequent analysis.  
