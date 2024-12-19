
# enbl ensemble object

# library(xgboost)


#' @name Ensbl
#' @title An ensemble of xgboosts
#' @description Each member of the ensemble is an xgboost, and contains a random sample of the data.
#'
#' @details
#' The ensbl object contains a list of xgboost objects
#'
#'
Ensemble <- R6Class("Ensemble",
                public = list(
                  bstl = list(),    # booster list
                  name = NULL,      # name of this member
                  obj_mode = NULL,  # either 'ensemble' or 'final'
                  data_mode = NULL, # what type of features are we making
                  size  = NULL,     # number of xgboost predictors
                  sample_prop = NULL, # percent of data to sample
                  feature_prop = NULL,
                  features_samp = NULL,
                  train_data = NULL, # the data to train from
                  idx = NULL,        # subsetting of the data 
                  test_data = NULL,  # will be filled by test data
                  pair_list = NULL,  # a char vector of genes
                  signatures = NULL, # the list of gene sets
                  label = NULL,      # the label vector 
                  params = NULL,     # parameters to train xgboost
                  nrounds = NULL,    # number of rounds of training
                  early_stopping_rounds = NULL, # number of rounds before early stopping
                  nthreads = NULL,   # number of threads to use
                  verbose = NULL,    # verbose statements printed
                  preds = NULL,      # predictions made, as list
                  pred_table = NULL, # table of predictions
                  pred_combined = NULL, # combined predictions
                  combine_function = NULL, # the function used to combine scores
                  
                  initialize = function(name,
                                        obj_mode,
                                        size, 
                                        data_mode,
                                        train_data, 
                                        idx,
                                        pair_list,
                                        signatures,
                                        label, 
                                        params) {
                    self$name <- name
                    self$obj_mode <- obj_mode
                    self$size <- size
                    self$data_mode <- data_mode
                    
                    # subset if we're in CV #
                    if (is.null(idx)) {
                      self$train_data <- train_data
                      self$label <- label 
                    } else {
                      self$train_data <- train_data[idx,]
                      self$label <- label[idx] 
                    }
                    
                    self$test_data <- NULL
                    self$pair_list <- pair_list
                    self$signatures <- signatures
                    self$combine_function <- params[['combine_function']]
                    self$nrounds <- params[['nrounds']]
                    self$early_stopping_rounds <- params[['early_stopping_rounds']]
                    self$nthreads <- params[['nthreads']]
                    self$verbose <- params[['verbose']]
                    self$feature_prop <- params[['feature_prop']]
                    self$sample_prop <- params[['sample_prop']]
                    self$params <- params
                    self$preds = list()
                    self$params[['nrounds']] <- NULL  # null these before passing them
                    self$params[['nthreads']] <- NULL # to the xgboost model
                    self$params[['verbose']] <- NULL
                    self$params[['size']] <- NULL
                    self$params[['sample_prop']] <- NULL
                    self$params[['feature_prop']] <- NULL
                    self$params[['combine_function']] <- NULL
                  },
                  
                  
                  print = function(...) {
                    paste0('ensemble: ', self$name ) 
                  },
                  

                  # data engineering
                  #' @description Data engineering, replaces the object's data.table.
                  data_eng = function(data_source=NULL) {
                    
                    # create a new data engineering object
                    this_deng <- Data_eng$new(self$data_mode, self$signatures, self$pair_list)
                                        
                    if (data_source == 'train') {
                      self$train_data <- this_deng$data_eng(self$train_data)
                    } else if (data_source == 'test') {
                      self$test_data <- this_deng$data_eng(self$test_data)
                    } else {
                      stop('ERROR: data source must be train, test.')
                    }
                  },


                  # Each member of the ensemble has a sample of the 
                  # training data, the proportion specified by "perc"
                  # or percentage.
                  sample_data = function(sample_perc, feature_perc) {
                    
                    # make the data list
                    res0 <- list()
                    
                    if (sample_perc < 1.0) {
                      ## generate random index
                      idx <- sample.int(n = nrow(self$train_data), 
                                        size = sample_perc*nrow(self$train_data), 
                                        replace = F)
                      jdx <- sample.int(n = ncol(self$train_data),
                                        size = feature_perc*ncol(self$train_data),
                                        replace=F)
                      
                      ## save the subset columns ##
                      self$features_samp <- colnames(self$train_data)[jdx]
                      
                      res0[['data']] <- as.matrix(self$train_data[idx, ..jdx])
                      res0[['label']] <- as.vector(self$label[idx])
                    } else {
                      res0[['data']] <- as.matrix(self$train_data)
                      res0[['label']] <- as.vector(self$label)
                    } 
                    return(res0)
                  },
                  
                  
                  train_models = function() {
                    # for each classifier from 1:size
                    #    train classifier randomly sampling with rate "perc"
                    for (i in 1:self$size) {
                      
                      # xgboost compains about this params member                      
                      p2 <- within(self$params, rm('early_stopping_rounds')) 
                      
                      if (self$obj_mode != 'final') {
                        
                        # sub-sample the data
                        sdat  <- self$sample_data(self$sample_prop, self$feature_prop)
                        
                        n_classes <- length(unique(sdat[['label']]))
                        
                        dtrain <- xgb.DMatrix(data=sdat[['data']], 
                                              label = sdat[['label']],
                                              nthread=self$nthreads)
                        
                        self$bstl[[i]] <- xgboost(params=p2, 
                                                  data=dtrain, 
                                                  nrounds=self$nrounds,
                                                  early_stopping_rounds=self$early_stopping_rounds,
                                                  verbose = self$verbose)
                      } else {
                        # it's multiclass final 
                        
                        # sub-sample the data
                        sdat  <- self$sample_data(1.0, 1.0)
                        
                        n_classes <- length(unique(sdat[['label']]))
                        
                        dtrain <- xgb.DMatrix(data=sdat[['data']], 
                                              label = sdat[['label']],
                                              nthread=self$nthreads)
                        
                        p2[['num_class']] <- n_classes
                        
                        self$bstl[[i]] <- xgboost(params=p2, 
                                                  data=dtrain, 
                                                  nrounds=self$nrounds,
                                                  early_stopping_rounds=self$early_stopping_rounds,
                                                  verbose = self$verbose)
                        
                      }
                    }
                  },

                  
                  ensemble_combine = function() {
                    
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
                    
                    combine_function <- self$combine_function
                    
                    if (combine_function == 'max') {
                      self$pred_combined <- apply(self$pred_table, 1, max)
                    }
                    else if (combine_function == 'mean') {
                      self$pred_combined <- apply(self$pred_table, 1, mean)
                    }
                    else if (combine_function == 'median') {
                      self$pred_combined <- apply(self$pred_table, 1, median)
                    }
                    else if (combine_function == 'majority') {
                      self$pred_combined <- apply(self$pred_table, 1, majv)
                    }
                    else if (combine_function == 'weighted') {
                      self$pred_combined <- apply(self$pred_table, 1, weif)
                    }
                    else {
                      print('SELECT max, mean, median, majority, weighted')
                    }
                  },
                  
                  
                  
                  final_ensemble_combine = function() {
                    
                    voting <- function(x) {
                      tablex <- table(x)
                      vote <- as.integer(names(which(tablex == max(tablex))))
                      if (length(vote) > 1) {
                        return(sample(vote,1))
                      }
                      else {
                        return(vote)
                      }
                    }
                    
                    # can be majority voting
                    res0 <- c()
                    for (i in 1:nrow(self$pred_table)){
                      res0 <- c(res0, as.numeric(voting(as.numeric(self$pred_table[i,]))))
                    }
                    self$pred_combined <- res0
                  },
                
                  
                  
                  member_predict = function(data) {
                    
                    combine_function <- self$combine_function
                    
                    if (all(class(data)[1] == 'matrix') == FALSE) {
                      data <- as.matrix(data)
                    }
                    
                    # for each ensemble member, make predictions using the feature subset
                    for (i in 1:self$size) {
                      features_samp <- self$bstl[[i]]$feature_names
                      # make the sub sampled data
                      pred_data <- data[, features_samp]
                      # make a prediction on this data
                      self$preds[[i]] <- predict(self$bstl[[i]], pred_data)
                    }    
                    
                    if (self$name == 'final') { # then we might have have multiclass calls.
                      # combine across member of the ensemble
                      self$pred_table <- do.call(cbind.data.frame, self$preds)
                      colnames(self$pred_table) <- sapply(1:self$size, function(a) paste0('ep',a))
                      self$pred_combined <- self$final_ensemble_combine()
                      
                    } else {
                      # then group all the predictions together
                      self$pred_table <- do.call(cbind.data.frame, self$preds)
                      colnames(self$pred_table) <- sapply(1:self$size, function(a) paste0('ep',a))
                      
                      # and make a final call or combine the predictions using a function
                      # then we combine all the predictions by applying the combine_function
                      self$pred_combined <- self$ensemble_combine()
                    }
                    
                  },
                  
                  
                  print_error = function(label, threshold) {
                    # for each member of the ensemble
                    for (i in 1:self$size) {
                      # check the error on the binary call
                      err <- as.numeric(sum(as.integer(self$preds[[i]] > threshold) != label)) / length(label)
                      print(paste0('ep', i, ' prediction error: ', err))
                    }
                    
                    err <- as.numeric(sum(as.integer(self$pred_combined > threshold) != label)) / length(label)
                    print(paste0('combined prediction error: ', err))
                  }
                  
                  
                  
                ) # end public
)
