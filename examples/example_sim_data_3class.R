
# Example using the autocv.

# This data was designed so that it can only (?) be predicted with paired 
# features after moving to a test set that is generated on an imaginary
# but very different platform with different levels of measures.

library(devtools)

tmp_lib <- "E:/Work/Code/tmp_lib"
dir.create(tmp_lib)

devtools::install_local("E:/Work/Code/robencla/", lib = tmp_lib)

## restart R

## explicitly load the affected packages from the temporary library
tmp_lib <- "E:/Work/Code/tmp_lib"
library(robencla, lib.loc = tmp_lib)


# our robust ensemble classifier machine
mod <- Robencla$new("Test1")

features <- c('X1','X2','X3','X4','X5','X6','X7','X8','X9','X10','X11','X12')

# xgboost parameters
params <- list(
  max_depth=6,    # "height" of the tree, 6 is actually default. I think about 12 seems better.  (xgboost parameter)
  eta=0.2,        # this is the learning rate. smaller values slow it down, more conservative   (xgboost parameter)
  nrounds=48,     # number of rounds of training, lower numbers less overfitting (potentially)  (xgboost parameter)
  early_stopping_rounds=2, # number of rounds without improvment stops the training (xgboost early_stopping_rounds)
  nthreads=4,     # parallel threads
  gamma=0.5,      # Minimum loss reduction required to again partition a leaf node. higher number ~ more conservative (xgboost parameter)
  lambda=1.5,     # L2 regularization term on weights, higher number ~ more conservative (xgboost parameter)
  alpha=0.5,      # L1 regularization term on weights. higher number ~ more conservative (xgboost parameter)
  size=11,        # Size of the ensemble, per binary prediction 
  train_perc=0.8, # The percentage of data used to train each ensemble member.
  combine_function='median',  # How the ensemble should be combined. Only median currently.
  verbose=0)

# this will train the classifier and test it on a small 20% split
mod$autocv(data_file='examples/data/sim_data_3classes_train.csv',
            label_name='label',
            sample_id=NULL,
            cv_rounds=5,
            data_mode=c('allpairs'), #'allpairs',  'pairs', 'sigpairs' 'quartiles', 'original', 'ranks',\
            signatures=NULL,
            pair_list=features,
            params=params)

# metrics on the test split
print("Across splits")
mod$classification_metrics() %>% print()


plot_pred_heatmap(mod, train_label = 'label_3', test_label = F)
