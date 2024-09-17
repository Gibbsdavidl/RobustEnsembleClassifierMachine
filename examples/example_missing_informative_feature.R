


# Example where most informative feature in training data is not 
# present in the test set.

#install.packages('~/Code/robencla', repos = NULL, type = 'source')

library(robencla)

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
  size=11,        # Size of the ensemble, per binary prediction 
  sample_prop=0.8, # The percentage of data used to train each ensemble member.
  feature_prop=0.8, # The percentage of data used to train each ensemble member.
  subsample=0.8,   # the xgboost machines subsample at this rate. 
  combine_function='weighted',  # How the ensemble should be combined. Only median currently.
  verbose=0)


# split the data, train and test
mod$train(data_file='data/missing_informative_train_data.csv',
          label_name='label',
          sample_id = NULL,
          data_mode=c('pairs'), # allpairs, pairs, sigpairs,quartiles,tertiles,binary,ranks,original
          pair_list=my_pairs,
          signatures=NULL,
          params=params)


mod$predict(data_file='data/missing_informative_test_data.csv',
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
ensemble_rocs(mod, flip=F) # uses the last fold trained.

# 
# $label_2
# # A tibble: 264 × 4
# Feature   MedGain MedCover MedFreq
# <chr>       <dbl>    <dbl>   <dbl>
# 1 X7_X_X8    0.273    0.204  0.0412 
# 2 X8_X_X9    0.0869   0.0674 0.0178 
# 3 X9_X_X10   0.0659   0.0498 0.0407 

library(pheatmap)

idx <- order(mod$test_label)
l1preds <- mod$ensbl$label_1$pred_table
labeldf <- data.frame(label=mod$test_label)
rownames(l1preds) <- sapply(1:500, function(a) paste0('x',a))
rownames(labeldf) <- sapply(1:500, function(a) paste0('x',a))
pheatmap(l1preds[idx,], annotation_row = labeldf, cluster_rows = T)

l2preds <- mod$ensbl$label_2$pred_table
labeldf <- data.frame(label=mod$test_label)
rownames(l2preds) <- sapply(1:500, function(a) paste0('x',a))
rownames(labeldf) <- sapply(1:500, function(a) paste0('x',a))
pheatmap(l2preds, annotation_row = labeldf)

l3preds <- mod$ensbl$label_3$pred_table
labeldf <- data.frame(label=mod$test_label)
rownames(l3preds) <- sapply(1:500, function(a) paste0('x',a))
rownames(labeldf) <- sapply(1:500, function(a) paste0('x',a))
pheatmap(l3preds, annotation_row = labeldf)

idx <- order(mod$test_label)
fpreds <- mod$pred_table[idx,]
labeldf <- data.frame(label=mod$test_label[idx])
rownames(fpreds) <- sapply(1:500, function(a) paste0('x',a))
rownames(labeldf) <- sapply(1:500, function(a) paste0('x',a))
pheatmap(fpreds, annotation_row = labeldf, cluster_rows = F)



mod$ensbl$label_1$bstl[[10]]$feature_names
mod$ensbl$label_1$bstl[[1]]$feature_names

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



