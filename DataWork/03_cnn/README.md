# Predict Nighttime Lights using Daytime Imagery

### Setup/Functions code
* __config.py:__ Defines filepaths
* __feature_extraction:__ Defines functions used in prepping data for CNN and in extracting CNN features to locations

### Data processing code
* __01_prep_data_for_cnn.py:__ Prep daytime and nighttime light data for CNN. Makes numpy arrays that can be fed into CNN. 
* __02_cnn.ipynb:__ CNN predicting NTL on DTL
* __03_cnn_results:__ Figure showing CNN results
* __04_extract_cnn_features_to_opm.py:__ Extract CNN features to OPM survey locations
* __05_cnn_features_pca.py:__ Performs PCA to reduce dimensionality of CNN features
* __06_extract_activation_maps.py:__ Extract activation maps of CNN features to illustrate what these features are generating. [INCOMPLETE]


