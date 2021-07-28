# Pakistan Poverty Estimation Results

tables_dir <- file.path(project_file_path, 'Outputs', 'Results', 'Tables')
figures_dir <- file.path(project_file_path, 'Outputs', 'Results', 'Figures')

# Load Data --------------------------------------------------------------------
results_df <- read.csv(file.path(data_dir, "DHS", "FinalData", "results", "results_fbonly.csv"))
ypred_df <- fread(file.path(data_dir, "DHS", "FinalData", "results", "ypred_fbonly.csv"))

