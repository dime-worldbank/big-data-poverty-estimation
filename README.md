# Replication Package for: Global poverty estimation using private and public sector big data sources

## To replicate analysis

1. Clone this repository
2. Download data from [here](LINKHERE)
3. In [`_main.R`](https://github.com/dime-worldbank/big-data-poverty-estimation/blob/master/_main.R), change `dropbox_dir` to point to the data folder and `github_dir` to point to the github repo.
4. Run [`_main.R`](https://github.com/dime-worldbank/big-data-poverty-estimation/blob/master/_main.R).

## Code

#### Main Script

* [`_main.R`](https://github.com/dime-worldbank/big-data-poverty-estimation/blob/master/_main.R): Main script that runs all code for project.

At the beginning of the `_main.R` script, three parameters are set at the beginning.

1. `RUN_CODE`: If `TRUE`, runs other scripts (eg, creating figures and tables). If `FALSE`, just loads packages and sets filepaths.
2. `DELETE_ML_RESULTS`: A large number of machine learning models are run for the analysis. The script that implements the machine learning analysis checks which models have already been run and only runs models that have not yet been run. All machine learning results come with provided data. Consequently, by default, the code will see all machine learning results and skip running machine models. By setting `DELETE_ML_RESULTS` to `TRUE`, machine learning results will be deleted, and machine learning models will be re-run. _NOTE:_ Machine learning results can take over 15 hours to run.
3. `EXPORT_TXT_REPORT_CODE_DURATION`: If set to `TRUE`, a .txt file will be exported that indicates how long the code took to run. 

The main script is set up to run code starting from cleaned data; specifically, it starts from a dataset of survey locations, with (1) wealth variables from survey data and (2) variables from other datasets (satellite, Facebook, OpenStreetMaps, etc) extracted to the survey location. Scripts are provided that clean the survey data and extract these variables; however, this repository and main script are set up to start from the cleaned data. The main script calls scripts to run machine learning models, clean these results, and produce figures and tables. 

The main script produces all figures and tables for the paper, with one minor exception; the main script does not produce the [figure with example daytime satellite images](https://github.com/dime-worldbank/big-data-poverty-estimation/blob/master/Paper%20Tables%20and%20Figures/figures/example_daytime_images.png). [This script](https://github.com/dime-worldbank/big-data-poverty-estimation/blob/33cbed1be65afcc50b373f88c0835df8078bac22/DataWork/02_get_process_ancillary_data/CNN%20Features%20Predict%20NTL/example_images.ipynb#L599) produces the figure, but the figure requires satellite data to be downloaded, which can be done using [this script](https://github.com/dime-worldbank/big-data-poverty-estimation/blob/33cbed1be65afcc50b373f88c0835df8078bac22/DataWork/02_get_process_ancillary_data/CNN%20Features%20Predict%20NTL/01_create_ntlgroup_tfrecord_name_ntlharmon.R).  

#### Organization

The [`DataWork`](https://github.com/dime-worldbank/big-data-poverty-estimation/tree/master/DataWork) contains code for data cleaning and analysis. Within `DataWork`, directories are organized as follows:

* `00_download_gadm`: Downloads [GADM](https://gadm.org/) data that is used in cleaning survey data.
* `01_clean_dhs`: Cleans DHS survey data.
* `01_clean_dhs_nga_experiment`: Cleans DHS survey data for Nigeria, using four rounds of data (used for ``Application: Estimating Wealth in Different Years'' section of paper)
* `01_clean_lsms`: Cleans LSMS survey data.
* `02_get_process_ancillary_data`: Extracts and process data around survey locations, such as from satellites, OpenStreetMaps, and Facebook Marketing data.
* `03_merge_ancillary_data_with_survey`: Merge ancillary data (satellite, OSM, Facebook data) extracted in previous step to survey data; creates cleaned, analysis-ready datasets.
* `04_poverty_estimation`: Implements poverty estimation models and appends results.
* `05_figures_tables_global`: Produces figures and tables for paper.


