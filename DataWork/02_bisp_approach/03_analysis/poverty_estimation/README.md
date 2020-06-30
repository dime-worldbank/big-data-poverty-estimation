# First pass at ML prediction


## Directory structure

Quick notes on what each file in this directory is/does:

- `output/` holds the saved data in .pkl and .csv formats from running the grid
    - `results.csv` shows results from evaluating each trained regressor in terms of several metrics: $R^2$, MSE, and MAE
    - Other pickled regressor objects are omitted from Github due to large sizes
- `01_convert_data_to_csv.R` converts the satellite data (originally in .RDS format) and the BISP data (originally in .dta format) to a more Python-friendly CSV format.
- `02_merge_household_income.ipynb` compiles the BISP income data to household-level aggregates and merges it onto the satellite data.
- `03_predict_first_pass.ipynb` lays out each stage of a simple ML pipeline, from preprocessing to feature generation to training to evaluation, on a simple test grid of hyperparameters.
- `04_explore_results.ipynb` explores the results stored in `output/results.csv`
- `config.py` holds key paths, hyperparameter grids and other configuration variables used in `run_grid.py`.
- `ml_utils.py` holds additional utility functions and object definitions used in `run_grid.py`.
- `run_grid.py` is a Python script form of `03_predict_first_pass.ipynb` that can be easily run on a server without GUI access.
- `setup_ec2_instance.sh` is a quick shell script that I used to quickly set up EC2 instances with the correct environment and Python libraries after transferring the relevant files over.
