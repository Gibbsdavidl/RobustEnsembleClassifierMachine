
# robencla the rob(ust) en(semble) cla(ssifier)

### Robust feature engineering transforms data and trains an ensemble of XGboost classifiers.

Below, the first example demonstrates cross validation, and the second example shows how to train and test on separate files.

## Cross Validation 

Naming feature-pairs per label often works better than taking all pairs of features.

```
devtools::install_github("gibbsdavidl/robencla")

library(robencla)

# Our classifier object named Anne.  Hi Anne!
mod1 <- Robencla$new("cell_model")

# Defining the list of signatures to compute signature-pairs, must be composed of column names
sigs = list(Sig1=c('Uniformity of Cell Shape','Uniformity of Cell Size', 'Marginal Adhesion'), 
            Sig2=c('Bare Nuclei', 'Normal Nucleoli', 'Single Epithelial Cell Size'),
            Sig3=c('Bland Chromatin', 'Mitoses'))
            
# Now, create a list of features, and pairs will only be created from that list.
pair_list <- c('Uniformity of Cell Shape', 'Uniformity of Cell Size', 'Marginal Adhesion')

# XGBoost parameters to pass to each sub-classifier in the ensembles
params <- list(
               max_depth=6,    # "height" of the tree, 6 is actually default. I think about 12 seems better.  (xgboost parameter)
               eta=0.2,        # this is the learning rate. smaller values slow it down, more conservative   (xgboost parameter)
               nrounds=24,     # number of rounds of training, lower numbers less overfitting (potentially)  (xgboost parameter)
               early_stopping_rounds=2, # number of rounds without improvment stops the training (xgboost early_stopping_rounds)
               nthreads=4,     # parallel threads
               gamma=0.0,      # Minimum loss reduction required to again partition a leaf node. higher number ~ more conservative (xgboost parameter)
               lambda=1.0,     # L2 regularization term on weights, higher number ~ more conservative (xgboost parameter)
               alpha=0.0,      # L1 regularization term on weights. higher number ~ more conservative (xgboost parameter)
               size=11,        # Size of the ensemble, per binary prediction 
               train_perc=0.5, # The percentage of data used to train each ensemble member.
               combine_function='median'  # How the ensemble should be combined. Only median currently.
               verbose=0)
               ###More on the xgboost parameters: https://xgboost.readthedocs.io/en/latest/parameter.html

# CROSS VALIDATION:  split the data, train and test, repeat over folds, all samples get a prediction.

mod1$autocv(

  # The data to use for training
  data_frame=data.table::fread('data/bcp_train_data.csv', sep=',', header=T),

  # !!! OR !!! leave data_frame=NULL, and pass in a file name for the data to use for training
  data_file='data/bcp_train_data.csv', # subset of data/Breast Cancer Prediction.csv',
  
  # The name of the column containing the training label
  label_name='Class',
  
  # The column containing the sample ID
  sample_id = 'Sample code number',

  # The number of cross validation folds to predict on, ignores data_split if >1.
  cv_rounds=5,
            
  # The data transformation mode, set using  allpairs,pairs,sigpairs,quartiles,tertiles,binarize,ranks,original
  data_mode=c('sigpairs','allpairs'),  # pairs takes LR pairs from the pair_list, allpairs forms all pairs from pair_list
  
  # If using signature pairs, then the list defining the signatures, must be composed of column names
  signatures=sigs,
  
  # Use only these features for the pairs
  pair_list=pair_list,
  
  # The XGBoost parameters (and a few extra params)
  params=params
  )



# Now we can print the top of the results table.
# If using cross validation, the table will have an 
# entry for each sample, otherwise only for the test set.
print(
  df <- mod1$cv_results
  table(True=df$Label, Call=df$BestCalls)
  )

# Prediction metrics on the test set predictions
print(
  mod1$classification_metrics()
  )

# Get the importance of features in each ensemble member
# these are found by combining the importance measures across the ensemble
print(
  mod1$importance()
)

# Plot the ROC curves for each class
ensemble_rocs(mod1)

```


## Second, training and testing on separate files.


```
devtools::install_github("gibbsdavidl/robencla")

library(robencla)

# Our classifier object named Anne.  Hi Anne!
mod2 <- Robencla$new("train_test")

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
               early_stopping_rounds=2, # number of rounds without improvment stops the training (xgboost early_stopping_rounds)
               nthreads=4,     # parallel threads
               gamma=0.0,        # Minimum loss reduction required to again partition a leaf node. higher number ~ more conservative (xgboost parameter)
               lambda=1.0,     # L2 regularization term on weights, higher number ~ more conservative (xgboost parameter)
               alpha=0.0,      # L1 regularization term on weights. higher number ~ more conservative (xgboost parameter)
               size=11,        # Size of the ensemble, per binary prediction 
               train_perc=0.5, # The percentage of data used to train each ensemble member.
               combine_function='median'  # How the ensemble should be combined. Only median currently.
               verbose=0)
               ###More on the xgboost parameters: https://xgboost.readthedocs.io/en/latest/parameter.html

# First we use the training data
mod2$train (data_frame=data.table::fread('data/bcp_train_data.csv', sep=',', header=T),
               # !!! OR !!! leave data_frame=NULL, and pass in a file name for the data to use for training
               # data_file='data/bcp_train_data.csv', # subset of data/Breast Cancer Prediction.csv',
               label_name='Class',
               sample_id = 'Sample code number',
               data_mode=c('sigpairs','pairs'), # allpairs,pairs,sigpairs,quartiles,tertiles,binarize,ranks,original #
               signatures=sigs,
               pair_list=pair_list,  # subset to these genes.
               params=params)
              
# now we apply the classifier to a test set.
mod2$predict(
              data_frame=data.table::fread('data/bcp_test_data.csv', sep=',', header=T),
              ### OR ### data_file = 'data/bcp_test_data.csv',
              label_name='Class',
              sample_id = 'Sample code number')


df <- mod2$results(include_label = T)

# create a confusion matrix
print(
  table(Label=df$Label, Pred=df$BestCalls)
)

# Prediction metrics on the test set predictions
print(
  mod2$classification_metrics(use_cv_results = F) # does not use CV results
)

# Get the importance of features in each ensemble member
# these are found by combining the importance measures across the ensemble
print(
  mod2$importance()
)

# Plot the ROC curves for each class
ensemble_rocs(mod2, flip=F)  ### Flip it if the binary labels are "upside down".

```


Let me know if something's not working!
  david.gibbs@systemsbiology.org
  gibbsdavidl@gmail.com
#
