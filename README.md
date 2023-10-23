# Replication Package for: Global poverty estimation using private and public sector big data sources

## To replicate analysis

1. Clone this repository
2. Download data from [here](LINKHERE)
3. In [`_main.R`](https://github.com/dime-worldbank/big-data-poverty-estimation/blob/master/_main.R), change `dropbox_dir` to point to the data folder and `github_dir` to point to the github repo.
4. Run [`_main.R`](https://github.com/dime-worldbank/big-data-poverty-estimation/blob/master/_main.R).

## Code

#### Main Script

* [`_main.R`](https://github.com/dime-worldbank/big-data-poverty-estimation/blob/master/_main.R): Main script that runs all code for project.

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


