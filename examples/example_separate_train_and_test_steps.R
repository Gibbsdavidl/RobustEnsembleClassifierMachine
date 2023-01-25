library(devtools)

tmp_lib <- "E:/Work/Code/tmp_lib"
dir.create(tmp_lib)

devtools::install_local("E:/Work/Code/robencla/", lib = tmp_lib)

## restart R

## explicitly load the affected packages from the temporary library
options(error = function() traceback(3))
tmp_lib <- "E:/Work/Code/tmp_lib"
library(robencla, lib.loc = tmp_lib)

########### OR START FROM GITHUB ############

#devtools::install_github("gibbsdavidl/robencla")

library(robencla)

# Our classifier object named Anne.  Hi Anne!
anne <- Robencla$new("Anne")

# Defining the list of signatures to compute signature-pairs, must be composed of column names
sigs = list(Sig1=c('Uniformity of Cell Shape','Uniformity of Cell Size', 'Marginal Adhesion'), 
            Sig2=c('Bare Nuclei', 'Normal Nucleoli', 'Single Epithelial Cell Size'),
            Sig3=c('Bland Chromatin', 'Mitoses'))

# Now, create a list of features, and pairs will only be created from that list.
pair_list <- c('Uniformity of Cell Shape','Uniformity of Cell Size', 'Marginal Adhesion')

# XGBoost parameters to pass to each sub-classifier in the ensembles
params <- list(
  max_depth=6,    # "height" of the tree, 6 is actually default. I think about 12 seems better.  (xgboost parameter)
  eta=0.2,        # this is the learning rate. smaller values slow it down, more conservative   (xgboost parameter)
  nrounds=24,     # number of rounds of training, lower numbers less overfitting (potentially)  (xgboost parameter)
  nthreads=4,     # parallel threads
  gamma=1,        # Minimum loss reduction required to again partition a leaf node. higher number ~ more conservative (xgboost parameter)
  lambda=1.5,     # L2 regularization term on weights, higher number ~ more conservative (xgboost parameter)
  alpha=0.5,      # L1 regularization term on weights. higher number ~ more conservative (xgboost parameter)
  size=11,        # size of the ensemble
  train_perc=0.5, # percent of data to sample for each member of the ensemble
  combine_function='median', # how to combine the ensemble
  verbose=0)
###More on the xgboost parameters: https://xgboost.readthedocs.io/en/latest/parameter.html

# First we use the training data
anne$autotrain(data_frame=data.table::fread('data/bcp_train_data.csv', sep=',', header=T),
               # !!! OR !!! leave data_frame=NULL, and pass in a file name for the data to use for training
               # data_file='data/bcp_train_data.csv', # subset of data/Breast Cancer Prediction.csv',
               label_name='Class',
               sample_id = 'Sample code number',
               data_mode=c('sigpairs','pairs'), # pairs,sigpairs,quartiles,tertiles,binarize,ranks,original #
               signatures=sigs,
               pair_list=pair_list,  # subset to these genes.
               params=params)

# now we apply the classifier to a test set.
anne$autotest(
  data_frame=data.table::fread('data/bcp_test_data.csv', sep=',', header=T),
  ### OR ### data_file = 'data/bcp_test_data.csv',
  label_name='Class',
  sample_id = 'Sample code number')


df <- anne$results(include_label = T)

# create a confusion matrix
print(
  table(Label=df$Label, Pred=df$BestCalls)
)

# Prediction metrics on the test set predictions
print(
  anne$classification_metrics(use_cv_results = F) # does not use CV results
)

# Get the importance of features in each ensemble member
# these are found by combining the importance measures across the ensemble
print(
  anne$importance()
)

# Plot the ROC curves for each class
ensemble_rocs(anne, flip=F)  ### Flip it if the binary labels are "upside down".