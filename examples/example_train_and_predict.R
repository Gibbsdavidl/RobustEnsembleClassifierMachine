


# Example using the autopred (auto-prediction) function.

install.packages('~/Code/robencla', repos = NULL, type = 'source')

library(robencla)

mod <- Robencla$new("Test3")

mod$version()

# list of signatures to compare
sigs = list(Sig1=c('Uniformity of Cell Shape','Uniformity of Cell Size', 'Marginal Adhesion'), 
            Sig2=c('Bare Nuclei', 'Normal Nucleoli', 'Single Epithelial Cell Size'),
            Sig3=c('Bland Chromatin', 'Mitoses'))

# only pair these features
my_pairs <- c('Clump Thickness','Uniformity of Cell Size','Uniformity of Cell Shape','Marginal Adhesion')

# xgboost parameters
params <- list(
  max_depth=6,    # "height" of the tree, 6 is actually default. I think about 12 seems better.  (xgboost parameter)
  eta=0.3,        # this is the learning rate. smaller values slow it down, more conservative   (xgboost parameter)
  nrounds=48,     # number of rounds of training, lower numbers less overfitting (potentially)  (xgboost parameter)
  early_stopping_rounds=2, # number of rounds without improvment stops the training (xgboost early_stopping_rounds)
  nthreads=4,     # parallel threads
  gamma=0.0,      # Minimum loss reduction required to again partition a leaf node. higher number ~ more conservative (xgboost parameter)
  lambda=1.0,     # L2 regularization term on weights, higher number ~ more conservative (xgboost parameter)
  alpha=0.0,      # L1 regularization term on weights. higher number ~ more conservative (xgboost parameter)
  size=11,        # Size of the ensemble, per binary prediction 
  sample_prop=0.8, # The percentage of data used to train each ensemble member.
  feature_prop=0.6, 
  combine_function='median',  # How the ensemble should be combined. Only median currently.
  verbose=0)


# split the data, train and test
mod$train(data_file='examples/data/bcp_train_data.csv',
              label_name='Class',
              sample_id = 'Sample code number',
              data_mode=c('allpairs', 'sigpairs'), # allpairs, pairs, sigpairs,quartiles,tertiles,binary,ranks,original
              pair_list=my_pairs,
              signatures=sigs,
              params=params)


mod$predict(data_file='examples/data/bcp_test_data.csv',
              label_name='Class',
              sample_id = 'Sample code number',
              )



# print the test data results table
mod$results(include_label = T) %>% head() %>% print()

# get a confusion matrix
table(Label=mod$test_label, Pred=mod$results(include_label = T)$BestCalls)

# metrics on the test set predictions
mod$classification_metrics(use_cv_results = F) %>% print()

# and get the importance of features in each ensemble member
mod$importance() %>% print()

# plot the ROC curves for each class
## IF THE ROC IS UPSIDE DOWN, SET FLIP=T
ensemble_rocs(mod, flip=F) # uses the last fold trained.

