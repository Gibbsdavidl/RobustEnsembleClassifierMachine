
# Example using the autocv.

# This data was designed so that it can only (?) be predicted with paired 
# features after moving to a test set that is generated on an imaginary
# but very different platform with different levels of measures.

tmp_lib <- "E:/Work/Code/tmp_lib"
dir.create(tmp_lib)
devtools::install_local("E:/Work/Code/robencla/", lib = tmp_lib, force = T, upgrade='never')
## restart R

## explicitly load the affected packages from the temporary library
library(robencla, lib.loc = tmp_lib)

## OR from github ##
#devtools::install_github('gibbsdavidl/robencla', ref ="allpairs_within", force = T, upgrade='never')
#library(robencla)


# define the features to be used in prediction
features <- c('X1','X2','X3','X4','X5','X6','X7','X8','X9','X10','X11','X12')
# or a list of features per class
feature_list <- list(
  label_1=c('X1','X2','X3','X9','X7'),
  label_2=c('X4','X5','X6','X10'),
  label_3=c('X7','X8','X11','X12')
)

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
  sample_prop=0.9, # The percentage of data used to train each ensemble member.
  feature_prop=1.0,
  subsample=0.9,
  combine_function='median',  # How the ensemble should be combined. 
  verbose=0)

mod <- Robencla$new("mod1")

# this will train the classifier and test it on a small 20% split
mod$autocv(data_file='examples/data/sim_data_3classes_train.csv',
            label_name='label',
            sample_id=NULL,
            cv_rounds=5,
            data_mode=c('allpairs'), #'allpairs',  'pairs', 'sigpairs' 'quartiles', 'original', 'ranks',\
            signatures=NULL,
            pair_list=feature_list,
            params=params)


# print the test data results table
mod$cv_results %>% head() %>% print()

# metrics on the test set predictions
mod$classification_metrics(use_cv_results = T) %>% print()

# get a confusion matrix
table(Label=mod$cv_results$Label, Pred=mod$cv_results$BestCalls)

# and get the importance of features in each ensemble member
mod$importance() %>% print()



# plot the ROC curves for each class
## IF THE ROC IS UPSIDE DOWN, SET FLIP=T
ensemble_rocs(mod) # uses the last fold trained.

# The final scores
plot_pred_final(mod)

# scores for each label
plot_pred_heatmap(mod, 
                  label = 'label_1',
                  cluster = T)

plot_pred_heatmap(mod, 
                  label = 'label_2',
                  cluster = T)

