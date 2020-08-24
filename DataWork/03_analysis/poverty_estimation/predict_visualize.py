# predict_visualize.py
#
# Description:
# Determines the best model from grid search, generates predictions,
# and creates visualizations.

import os, ast, math
import numpy as np
import pandas as pd
import geopandas as gpd

from sklearn.svm import LinearSVC
from sklearn.tree import DecisionTreeClassifier
from sklearn.ensemble import (BaggingClassifier, AdaBoostClassifier, 
                              GradientBoostingClassifier, RandomForestClassifier)
from sklearn.neighbors import KNeighborsClassifier
from sklearn.naive_bayes import GaussianNB
from sklearn.metrics import (accuracy_score, precision_score, recall_score,
                             f1_score, classification_report, confusion_matrix)

import matplotlib.pyplot as plt
import seaborn as sns
import contextily as ctx
import matplotlib.cm as cm

import warnings
warnings.filterwarnings('ignore')

import config as cf
from ml_utils import TrainedRegressor

RESULTS = os.path.join(cf.CURRENT_DIRECTORY, 'output/results.csv')
TARGET_NAME = cf.TARGET_NAME
CURRENT_DIRECTORY = cf.CURRENT_DIRECTORY
PAKISTAN_BOUNDARIES_SHAPEFILE = cf.PAKISTAN_BOUNDARIES_SHAPEFILE


def get_best_model():
    '''
    Grabs best model according to grid search.
    '''
    # Load grid search results
    results = pd.read_csv(RESULTS)
    results_sorted = results.sort_values(['final_score', 'accuracy_score'], ascending=False)
    
    # Load best regressor by accuracy
    df_bestmodel = results_sorted.iloc[0]
    best_regressor = pd.read_pickle(f'output/{df_bestmodel["regressor"]}_trained.pkl')
    
    # filename = f'output/{df_bestmodel["regressor"]}_trained.pkl'
    # filepath = os.path.join('/Users/nguyenluong/wb_internship/Data/', filename)
    # best_regressor = pd.read_pickle(filepath)
    
    # Grab best model
    best_model = None
    for x in best_regressor:
        if df_bestmodel['params'] == x.params and df_bestmodel['feature_group'] == x.features:
            best_model = x
    
    return df_bestmodel, best_model


def get_predictions(df, best_model):
    '''
    Predicts binary poverty for entire dataset.
    '''
    # Merge manipulated bisp data and fully prepped data to get geometry for predictions
    # original = pd.read_pickle(bisp_with_coords)[['uid', 'geometry']]
    # original = gpd.GeoDataFrame(original, geometry='geometry')
    # fully_prepped = pd.read_pickle(fully_prepped_data)

    # df = original.merge(fully_prepped, left_on='uid', right_on='uid')
    # df = df.rename(columns={TARGET_NAME: 'true_label'})
    # df = df[feature_columns + ['uid', 'geometry', 'true_label']]

    # Run best model and get predictions
    x = df.drop(columns=['uid', 'geometry', 'true_label'])
    df['pred_label'] = best_model.regressor.predict(x)

    return df['true_label'], df['pred_label']


def fig1_eval_metrics(df_bestmodel, true_labels, pred_labels):
    # Grid Search
    cr = {'Not in Poverty': ast.literal_eval(df_bestmodel['nonpoverty_class']),
          'In Poverty': ast.literal_eval(df_bestmodel['poverty_class'])}
    df_cr = pd.DataFrame.from_dict({i: cr[i] for i in cr.keys()}, orient='index')
    df_cr = df_cr.drop(columns=['support'])

    # Whole data
    cr1 = classification_report(true_labels, pred_labels, \
                                target_names=['Not in Poverty','In Poverty'], \
                                output_dict=True)
    df_cr1 = pd.DataFrame.from_dict({k: cr1[k] for k in ('Not in Poverty','In Poverty')}).T
    df_cr1 = df_cr1.drop(columns=['support'])

    # Plot
    f, (ax0, ax1) = plt.subplots(figsize=(14, 5), nrows=1, ncols=2)
    plt.suptitle('Evaluations Metrics by Label', fontsize=20)
    sns.set(context="paper")
    sns.heatmap(df_cr, annot=True, cmap=sns.cubehelix_palette(30), ax=ax0, vmin=0.5, vmax=1)
    ax0.set_xlabel('Evaluation Metrics', fontsize=12)
    ax0.set_ylabel('Label', fontsize=12)
    ax0.set_title('On Testing Data', fontsize=15)
    sns.heatmap(df_cr1, annot=True, cmap=sns.cubehelix_palette(30), ax=ax1, vmin=0.5, vmax=1)
    ax1.set_xlabel('Evaluation Metrics', fontsize=12)
    ax1.set_ylabel('Label', fontsize=12)
    ax1.set_title('On Predictions', fontsize=15)
    plt.savefig('grid_eval_metrics.png', bbox_inches='tight')


def fig2_confusion_matrix(true_labels, pred_labels):
    # Compute confusion matrix and define classes/names
    cm = confusion_matrix(true_labels, pred_labels)

    # Format box text
    classes = ['Not in Poverty', 'In Poverty']
    names = ['True Negative','False Positive','False Negative', 'True Positive']
    labels = ["{}\n".format(x) for x in names]
    counts = ["{0:0.0f}\n".format(x) for x in cm.flatten()]
    percentages = ["{0:.2%}".format(x) for x in cm.flatten()/np.sum(cm)]
    box_text = [f'{x}{y}{z}'.strip() for x, y, z in zip(labels, counts, percentages)]
    box_text = np.asarray(box_text).reshape(cm.shape)

    # Format stats 
    a = accuracy_score(true_labels, pred_labels)
    r = recall_score(true_labels, pred_labels)
    p = precision_score(true_labels, pred_labels)
    f1 = f1_score(true_labels, pred_labels)
    stats_text = "\n\nAccuracy={:0.3f}\nPrecision={:0.3f}\nRecall={:0.3f}\nF1 Score={:0.3f}".format(
                    a, p, r, f1)

    # Plot
    f, ax = plt.subplots(figsize=(7, 5))
    sns.set(context="paper")
    ax = sns.heatmap(cm, annot=box_text, fmt="", cmap="Blues", xticklabels=classes, yticklabels=classes)
    ax.set_xlabel(('Predicted Label' + stats_text), fontsize=12)
    ax.set_ylabel('True Label', fontsize=12)
    ax.set_title('Confusion Matrix', fontsize=15)
    plt.savefig('confusion_matrix.png', bbox_inches='tight')


def fig3_feat_importances(best_model, column_names):
    # Get importances
    importances = pd.Series(best_model.regressor.feature_importances_).sort_values(ascending=False)
    importances = importances.replace(np.inf, np.nan)
    importances = importances.dropna()[0:10]

    # Sort in descending order
    indices = importances.index.values

    # Sort the labels in a corresponding fashion
    labels = column_names[indices]

    # Plot
    f, ax = plt.subplots(figsize=(8, 8))
    sns.set(style="darkgrid", context="paper")
    ax = sns.barplot(x=importances, y=labels, palette="rocket")
    ax.set_ylabel("Features", fontsize=15)
    ax.set_xlabel("Level of Importances", fontsize=15)
    ax.set_title('Ten Most Salient Features', fontsize=20)
    plt.savefig('feature_importances.png', bbox_inches='tight')


def fig4_map(df):
    # Load boundaries
    boundaries = gpd.read_file(PAKISTAN_BOUNDARIES_SHAPEFILE)
    fig, (ax0, ax1) = plt.subplots(figsize=(14, 6), nrows=1, ncols=2)

    # True Labels 
    boundaries.plot(ax=ax0, color='ALICEBLUE', edgecolor='LIGHTSKYBLUE')
    df[df['true_label']==0].plot(column='true_label', color='MEDIUMSPRINGGREEN', edgecolor='MEDIUMSPRINGGREEN', ax=ax0)
    df[df['true_label']==1].plot(column='true_label', color='ORCHID', edgecolor='PLUM', ax=ax0)
    ctx.add_basemap(ax0, crs='epsg:4326', url=ctx.sources.OSM_C)
    ax0.set_title('True States of Poverty', fontsize=20)

    # Predictions
    boundaries.plot(ax=ax1, color='ALICEBLUE', edgecolor='LIGHTSKYBLUE')
    df[df['pred_label']==0].plot(column='pred_label', color='MEDIUMSPRINGGREEN', edgecolor='MEDIUMSPRINGGREEN', ax=ax1)
    df[df['pred_label']==1].plot(column='pred_label', color='ORCHID', edgecolor='PLUM', ax=ax1)
    ctx.add_basemap(ax1, crs='epsg:4326', url=ctx.sources.OSM_C)
    ax1.set_title('Predicted States of Poverty', fontsize=20)

    fig.legend(labels=['Not in Poverty', 'In Poverty'], loc='lower center')
    plt.savefig('map.png', bbox_inches='tight')


def predict_and_visualize(bisp_with_coords='bisp_with_coords.pkl',
                          fully_prepped_data='fully_prepped_data.pkl'):

    # SET DIRECTORY
    os.chdir(CURRENT_DIRECTORY)
    print('PREDICTING OUTCOMES AND GENERATION VISUALIZATIONS')

    # GET BEST MODEL
    df_bestmodel, best_model = get_best_model()
    print(f'Best Model: {best_model.method}')
    print(f'Feature Group: {best_model.features}')
    print(f'Parameters: {best_model.params}')
    print(f'Accuracy: {df_bestmodel["accuracy_score"]}')
    print(f'Precision: {df_bestmodel["precision_score"]}')
    print(f'Recall for HHs in Poverty: {df_bestmodel["recall_poverty"]}')
    print(f'Recall for HHs Not in Poverty: {df_bestmodel["recall_nonpoverty"]}')
    print(f'Final Score: {df_bestmodel["final_score"]}')

    # MERGE BISP WITH COORDS AND FULLY PREPPED DATA TO ATTACH GEOMETRY TO PREDICTIONS
    original = pd.read_pickle(bisp_with_coords)[['uid', 'geometry']]
    original = gpd.GeoDataFrame(original, geometry='geometry')
    fully_prepped = pd.read_pickle(fully_prepped_data)
    df = original.merge(fully_prepped, left_on='uid', right_on='uid')
    df = df.rename(columns={TARGET_NAME: 'true_label'})

    # SUBSET ACCORDING TO FEATURE GROUP OF BEST MODEL
    feature_columns = ast.literal_eval(df_bestmodel["feature_columns"])
    df = df[feature_columns + ['uid', 'geometry', 'true_label']]

    # GET PREDICTIONS
    true_labels, pred_labels = get_predictions(df, best_model)

    # GENERATE AND SAVE VISUALIZATIONS
    fig1_eval_metrics(df_bestmodel, true_labels, pred_labels)
    fig2_confusion_matrix(true_labels, pred_labels)
    fig3_feat_importances(best_model, np.array(feature_columns))
    fig4_map(df)


if __name__ == '__main__':
    predict_and_visualize()

