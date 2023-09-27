

### Testing feature sets per class

# each class has a set of features that will be predictive
# the features are paired, so that one of the pair will always
# have a higher value.
# 
# 

#devtools::install_github('gibbsdavidl/robencla')

nclasses <- 3
featsper <- 20
nsamples <- 200
nfeatures <- nclasses*featsper

labels <- sample(1:nclasses, size = nsamples, replace = T)
labels <- as.character(labels)

feature_names <- sapply(1:nfeatures, function(x){
  paste0("feat_",paste0(sample(1:9,size=4),collapse = ''))
})

sample_names <- sapply(1:nsamples, function(x){
  paste0("samp_",paste0(sample(1:9,size=4),collapse = ''))
})

pair_list <- lapply(0:(nclasses-1), function(x){
  print(x*featsper+1)
  print(x*featsper + featsper)
  feature_names[(x*featsper+1):(x*featsper+featsper)]
})

names(pair_list) = c('1','2','3')

featuremap <- lapply(0:(nclasses-1), function(x){
  c( (x*featsper+1):(x*featsper + featsper) )
})


gencol0 <- function(label, labels, dir) {
  idx <- which(label == labels)
  n <- length(idx)
  list(
    idx,
    rnorm(n, mean=0+dir),
    rnorm(n, mean=0)
  )
}


makeData <- function(nclasses, nsamples, nfeatures, labels,
                     feature_names, sample_names, 
                     offset1, offset2, uptrack, numpredfeat) {
  
  datmat <- mat.or.vec(nr = nsamples, nc = nfeatures)
  colnames(datmat) <- feature_names
  rownames(datmat) <- sample_names
  
  if (is.null(uptrack)) {
    uptrack <- sample(c(0,1), size=nfeatures, replace = T)
  }
  
  for(ci in 1:nclasses) {
    predcounter <- numpredfeat
    for (j in seq.int(1, nfeatures ,2)) {
      upclass <- which(sapply(featuremap, function(a) j %in% a))
      if (ci == upclass && predcounter > 0) {
        # this is a predictive feature
        thisdat <- gencol0(ci, labels, offset1)
        # subtrack 1 from the informative features allowed
        predcounter <- predcounter-1
        if (uptrack[j] > 0) {
          datmat[thisdat[[1]],j]     <- thisdat[[2]]
          datmat[thisdat[[1]],(j+1)] <- thisdat[[3]]
        } else {
          datmat[thisdat[[1]],j]     <- thisdat[[3]]
          datmat[thisdat[[1]],(j+1)] <- thisdat[[2]]
        }
      } else {
        # not a predictive feature
        thisdat <- gencol0(ci, labels, offset2)
        datmat[thisdat[[1]],j]     <- thisdat[[2]]
        datmat[thisdat[[1]],(j+1)] <- thisdat[[3]]
      }
    }
  }
  datmat <- as.data.frame(datmat)
  datmat$Label   <- labels
  datmat$Barcode <- sample_names
  return(list(datmat,uptrack))  
}




#devtools::install_github('gibbsdavidl/robencla')

runTest1 <- function() {
  
  library(robencla)
  ################################################################
  
  res0 <- makeData(nclasses, nsamples, nfeatures, labels,
                   feature_names, sample_names, 3, 0, NULL, 2)
  train <- res0[[1]]
  uptrack <- res0[[2]]
  
  #################################################################
  labels2 <- sample(1:nclasses, size = nsamples, replace = T)
  
  sample_names2 <- sapply(1:nsamples, function(x){
    paste0("samp_",paste0(sample(1:9,size=4),collapse = ''))
  })
  
  res1  <- makeData(nclasses, nsamples, nfeatures, labels2,
                    feature_names, sample_names2, 3, 0, uptrack, 2)
  test <- res1[[1]]
  
  ################################################################
  
  
  # XGBoost parameters to pass to each sub-classifier in the ensembles
  params <- list(
    max_depth=6,    # "height" of the tree, 6 is actually default.                      (xgboost parameter)
    eta=0.3,        # this is the learning rate. smaller values slow it down, more conservative   (xgboost parameter)
    nrounds=15,     # number of rounds of training, lower numbers less overfitting (potentially)  (xgboost parameter)
    early_stopping_rounds=2,
    nthreads=4,     # parallel threads
    gamma=0,        # Minimum loss reduction required to again partition a leaf node. higher number ~ more conservative (xgboost parameter)
    lambda=1.0,     # L2 regularization term on weights, higher number ~ more conservative (xgboost parameter)
    alpha=0.0,      # L1 regularization term on weights. higher number ~ more conservative (xgboost parameter)
    size=11,        # Size of the ensemble, per binary prediction 
    train_perc=0.8, # The percentage of data used to train each ensemble member.
    combine_function='median',  # How the ensemble should be combined. Only median currently.
    verbose=0)
  ###More on the xgboost parameters: https://xgboost.readthedocs.io/en/latest/parameter.html
  
  
  # Our classifier object named Anne.  Hi Anne!
  mod1 <- Robencla$new("mod1")
  # First we use the training data
  mod1$autotrain(data_frame=train,
                 label_name='Label',
                 sample_id = 'Barcode',
                 data_mode=c('pairs'), # allpairs, pairs, sigpairs, quartiles, tertiles, binarize, ranks, original #
                 signatures=NULL,
                 pair_list=pair_list,  # subset to these genes.
                 params=params)
  
  
  # now we apply the classifier to a test set.
  mod1$autotest(
    data_frame=test,
    ### OR ### data_file = 'data/bcp_test_data.csv',
    label_name='Label',
    sample_id = 'Barcode')
  
  
  # Prediction metrics on the test set predictions
  print(
    mod1$classification_metrics(use_cv_results = F) # does not use CV results
  )
  
  print(
    table(Label=mod1$test_label, Pred=mod1$results()$BestCall)
  )
  
  return(list(mod1, train, test))
}

res2 <- runTest1()
train <- res2[[2]]
test <- res2[[3]]
###################################################################################

####  SWAP labels

feature_name_shuff <- sample(feature_names, size = length(feature_names), replace = F)
pair_list_shuff <- lapply(0:(nclasses-1), function(x){
  print(x*featsper+1)
  print(x*featsper + featsper)
  feature_name_shuff[(x*featsper+1):(x*featsper+featsper)]
})
names(pair_list_shuff) <- c('2','3','1')

mod1 <- Robencla$new("mod1")
# First we use the training data
mod1$autotrain(data_frame=train,
               label_name='Label',
               sample_id = 'Barcode',
               data_mode=c('pairs'), # allpairs, pairs, sigpairs, quartiles, tertiles, binarize, ranks, original #
               signatures=NULL,
               pair_list=feature_name_shuff,  # subset to these genes.
               params=params)


# now we apply the classifier to a test set.
mod1$autotest(
  data_frame=test,
  ### OR ### data_file = 'data/bcp_test_data.csv',
  label_name='Label',
  sample_id = 'Barcode')


# Prediction metrics on the test set predictions
print(
  mod1$classification_metrics(use_cv_results = F) # does not use CV results
)

print(
  table(Label=mod1$test_label, Pred=mod1$results()$BestCall)
)

##################################################################################



table(labels, train[,feature_names[1]] > train[, feature_names[2]])

table(labels2, test[,feature_names[1]] > test[, feature_names[2]])

table(labels, train[,feature_name_shuff[1]] > train[, feature_name_shuff[2]])

table(labels2, test[,feature_name_shuff[1]] > test[, feature_name_shuff[2]])


"feat_1396" "feat_9187"

table(labels, train[,"feat_1396"] > train[, "feat_9187"])
table(labels2, test[,"feat_1396"] > test[, "feat_9187"])

feat_9356_X_feat_1269


table(labels, train[,"feat_9356"] > train[, "feat_1269"])

table(labels, train[,feature_names[1]] > train[, feature_names[2]])


