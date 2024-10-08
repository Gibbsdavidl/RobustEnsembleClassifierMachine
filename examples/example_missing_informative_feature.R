


# Example where most informative feature in training data is not 
# present in the test set.

tmp_lib <- "E:/Work/Code/tmp_lib"
dir.create(tmp_lib)
devtools::install_local("E:/Work/Code/robencla/", lib = tmp_lib, force = T)
## restart R
## explicitly load the affected packages from the temporary library
library(robencla, lib.loc = tmp_lib)

## OR from github ##

devtools::install_github('gibbsdavidl/robencla',force = T)
library(robencla)


######################################################

mod <- Robencla$new("Test3")

mod$version()

# only pair these features
my_pairs <- sapply(1:24, function(i) paste0('X',i))

# xgboost parameters
params <- list(
  max_depth=12,    # "height" of the tree, 6 is actually default. I think about 12 seems better.  (xgboost parameter)
  eta=0.3,        # this is the learning rate. smaller values slow it down, more conservative   (xgboost parameter)
  nrounds=48,     # number of rounds of training, lower numbers less overfitting (potentially)  (xgboost parameter)
  early_stopping_rounds=2, # number of rounds without improvment stops the training (xgboost early_stopping_rounds)
  nthreads=4,     # parallel threads
  gamma=0.3,      # Minimum loss reduction  to again partition a leaf node. higher number ~ more conservative 
  lambda=1.3,     # L2 regularization term on weights, higher number ~ more conservative (xgboost parameter)
  alpha=0.3,      # L1 regularization term on weights. higher number ~ more conservative (xgboost parameter)
  size=15,        # Size of the ensemble, per binary prediction 
  sample_prop=0.8, # The percentage of data used to train each ensemble member.
  feature_prop=0.8, # The percentage of data used to train each ensemble member.
  subsample=0.8,   # the xgboost machines subsample at this rate. 
  combine_function='median',  # How the ensemble should be combined. Only median currently.
  verbose=0)


# split the data, train and test
mod$train(data_file='examples/data/missing_informative_train_data.csv',
          label_name='label',
          sample_id = NULL,
          data_mode=c('pairs'), # allpairs, pairs, sigpairs,quartiles,tertiles,binary,ranks,original
          pair_list=my_pairs,
          signatures=NULL,
          params=params)


mod$predict(data_file='examples/data/missing_informative_train_data.csv',
            label_name='label'
)



mod$predict(data_file='examples/data/missing_informative_test_data.csv',
            label_name='label'
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
ensemble_rocs(mod) # uses the last fold trained.

# The final scores
plot_pred_final(mod)


# scores for each label
plot_pred_heatmap(mod, label = 'label_1',
                  include_label = T, cluster = T)

plot_pred_heatmap(mod, label = 'label_2',
                  include_label = T, cluster = T)

plot_pred_heatmap(mod, label = 'label_3',
                  include_label = T, cluster = T)


### result of applying different combination functions.

# weighted mean
weif <- function(x) {
  x <- sort(x, decreasing = T)
  xsum <- tanh( sum( sapply(1:length(x), function(a) (1/(a))*x ) ) )
  return(xsum)
}

# majority vote
majv <- function(x) {
  sum(x > 0.5)
}


l1preds <- mod$ensbl$label_1$pred_table
labeldf <- data.frame(label=mod$test_label)
boxplot(apply(l1preds, 1, mean) ~ labeldf$label, main='mean')
boxplot(apply(l1preds, 1, weif) ~ labeldf$label, main='weif', outline=F)
boxplot(apply(l1preds, 1, majv) ~ labeldf$label, main='maj')


l2preds <- mod$ensbl$label_2$pred_table
labeldf <- data.frame(label=mod$test_label)
boxplot(apply(l2preds, 1, mean) ~ labeldf$label, main='mean')
boxplot(apply(l2preds, 1, median) ~ labeldf$label, main='median')
boxplot(apply(l2preds, 1, max) ~ labeldf$label, main='max')


l3preds <- mod$ensbl$label_3$pred_table
labeldf <- data.frame(label=mod$test_label)
boxplot(apply(l3preds, 1, mean) ~ labeldf$label, main='mean')
boxplot(apply(l3preds, 1, median) ~ labeldf$label, main='median')
boxplot(apply(l3preds, 1, max) ~ labeldf$label, main='max')

cpreds <- mod$pred_table
cpreds$label <- mod$test_label



