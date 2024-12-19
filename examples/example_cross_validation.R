

# Example using the autopred (auto-prediction) function.

# Example where most informative feature in training data is not 
# present in the test set.

tmp_lib <- "E:/Work/Code/tmp_lib"
dir.create(tmp_lib)
devtools::install_local("E:/Work/Code/robencla/", lib = tmp_lib, force = T, upgrade='never')
## restart R
## explicitly load the affected packages from the temporary library
library(robencla, lib.loc = tmp_lib)

## OR from github ##

#devtools::install_github('gibbsdavidl/robencla', ref ="allpairs_within", force = T, upgrade='never')
#library(robencla)

######################################################



mod <- Robencla$new("Test2")

mod$version()

# list of signatures to compare
sigs = list(Sig1=c('Uniformity of Cell Shape','Uniformity of Cell Size', 'Marginal Adhesion'), 
            Sig2=c('Bare Nuclei', 'Normal Nucleoli', 'Single Epithelial Cell Size'),
            Sig3=c('Bland Chromatin', 'Mitoses'))

# only pair these features
features <- c('Clump Thickness','Uniformity of Cell Size','Uniformity of Cell Shape',
              'Marginal Adhesion','Single Epithelial Cell Size','Bare Nuclei','Bland Chromatin','Normal Nucleoli') # ,'Mitoses'

plist <- list('2'=c('Clump Thickness','Uniformity of Cell Size','Uniformity of Cell Shape','Marginal Adhesion'),
              '4'=c('Marginal Adhesion','Single Epithelial Cell Size','Bare Nuclei','Bland Chromatin','Normal Nucleoli','Mitoses'))

# xgboost parameters
params <- list(
  max_depth=12,    # "height" of the tree, 6 is actually default. I think about 12 seems better.  (xgboost parameter)
  eta=0.3,        # this is the learning rate. smaller values slow it down, more conservative   (xgboost parameter)
  nrounds=48,     # number of rounds of training, lower numbers less overfitting (potentially)  (xgboost parameter)
  early_stopping_rounds=2, # number of rounds without improvment stops the training (xgboost early_stopping_rounds)
  nthreads=4,     # parallel threads
  gamma=0.2,      # Minimum loss reduction required to again partition a leaf node. higher number ~ more conservative (xgboost parameter)
  lambda=1.2,     # L2 regularization term on weights, higher number ~ more conservative (xgboost parameter)
  alpha=0.2,      # L1 regularization term on weights. higher number ~ more conservative (xgboost parameter)
  size=11,        # Size of the ensemble, per binary prediction 
  sample_prop=0.8, # The percentage of data used to train each ensemble member.
  feature_prop=0.9,
  subsample=0.8,
  combine_function='median',  # How the ensemble should be combined. 
  verbose=0)

# split the data, train and test
mod$autocv(data_file='examples/data/Breast Cancer Prediction.csv',
             label_name='Class',
             sample_id = 'Sample code number',
             cv_rounds=3,
             data_mode=c('pairs'), # namedpairs, pairs, allpairs, sigpairs, quartiles, tertiles, binary, ranks, original
             pair_list=plist, # features
             signatures=NULL, # sigs
             params=params)

# print the test data results table
mod$cv_results %>% head() %>% print()

# metrics on the test set predictions
mod$classification_metrics() %>% print()

# get a confusion matrix
table(Label=mod$test_label, Pred=mod$results(include_label = T)$BestCalls)

# and get the importance of features in each ensemble member
mod$importance() %>% print()



# plot the ROC curves for each class
## IF THE ROC IS UPSIDE DOWN, SET FLIP=T
ensemble_rocs(mod) # uses the last fold trained.

# The final scores
plot_pred_final(mod)

# scores for each label
plot_pred_heatmap(mod, label = '2',
                  include_label = T, cluster = T)

plot_pred_heatmap(mod, label = '4',
                  include_label = T, cluster = T)




