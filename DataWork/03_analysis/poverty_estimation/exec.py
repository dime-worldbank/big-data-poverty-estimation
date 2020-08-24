# exec.py
#
# Description
# Runs entire process of 1) building and running cnn, 2) cleaning and loading 
# appropriate data, 3) running grid search to find best model, 4) generating
# outcomes with best model and visualizing results.

import os
import argparse
import pandas as pd
import config as cf
import cnn
import run_grid
import predict_visualize

GIT_DIRECTORY = cf.GIT_DIRECTORY
ARCHIVED_CNN = os.path.join(GIT_DIRECTORY, 'prepped_objects', 'archived_CNN.h5')
# GRIDSEARCH_READY_DATA = os.path.join(GIT_DIRECTORY, 'prepped_objects', 'gridsearch_ready_data.pkl')
# GRIDSEARCH_READY_FEATURE_GROUPS = os.path.join(GIT_DIRECTORY, 
#                                               'prepped_objects',
#                                               'gridsearch_ready_feature_groups.json')
# PREPPED_BISP_WITH_COORDS = os.path.join(GIT_DIRECTORY, 
#                                             'prepped_objects', 
#                                             'prepped_bisp_with_coords.pkl')


def main():

    parser = argparse.ArgumentParser(description='''Build and run cnn, clean and load,
        all appropriate data, run grid search, generate outcomes with best model,
        visualize results.''')
    # parser.add_argument('-cd',
    #                     default=False,
    #                     action='store_true',
    #                     help='Use archived CNN and prepped data')
    parser.add_argument('-c',
                        default=False,
                        action='store_true',
                        help='Use archived CNN')

    args = parser.parse_args()

    # if args.cd:
    #     print('''Using archived trained CNN and fully prepped data.''')
    #     run_grid.run_grid_search(cnn_filepath=ARCHIVED_CNN,
    #                              grid_ready_data=GRIDSEARCH_READY_DATA,
    #                              grid_ready_feature_groups=GRIDSEARCH_READY_FEATURE_GROUPS)
    #     predict_visualize.predict_and_visualize(bisp_with_coords=PREPPED_BISP_WITH_COORDS,
    #                                             fully_prepped_data=GRIDSEARCH_READY_DATA)

    if args.c:
        print('''Using archieved trained CNN and running grid search.''')
        run_grid.run_grid_search(cnn_filepath=ARCHIVED_CNN)
        predict_visualize.predict_and_visualize()

    else:
        print('Running entire process from scratch.')
        cnn.buid_and_run_cnn()
        run_grid.run_grid_search()
        predict_visualize.predict_and_visualize()


if __name__ == '__main__':
    main()
