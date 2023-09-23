
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
                  bstl = list(),   # booster list
                  name = NULL,     # name of this member
                  obj_mode = NULL, # either 'ensemble' or 'final'
                  data_mode = NULL, # what type of features are we making
                  size  = NULL,    # number of xgboost predictors
                  perc = NULL,     # percent of data to sample
                  train_data = NULL,  # the data to train from
                  test_data = NULL, # will be filled by test data
                  pair_list = NULL,  # a char vector of genes
                  label = NULL,    # the label vector 
                  params = NULL,   # parameters to train xgboost
                  nrounds = NULL,    # number of rounds of training
                  early_stopping_rounds = NULL, # number of rounds before early stopping
                  nthreads = NULL,   # number of threads to use
                  verbose = NULL,    # verbose statements printed
                  preds = NULL,    # predictions made, as list
                  pred_table = NULL,  # table of predictions
                  pred_combined = NULL, # combined predictions
                  combine_function = NULL, # the function used to combine scores
                  
                  initialize = function(name,
                                        obj_mode,
                                        size, 
                                        data_mode,
                                        train_data, 
                                        pair_list,
                                        signatures,
                                        label, 
                                        params) {
                    self$name <- name
                    self$obj_mode <- obj_mode
                    self$size <- size
                    self$data_mode <- data_mode
                    self$train_data <- train_data
                    self$test_data <- NULL
                    self$pair_list <- pair_list
                    self$signatures <- signatures
                    self$label <- label 
                    self$combine_function <- params[['combine_function']]
                    self$nrounds <- params[['nrounds']]
                    self$early_stopping_rounds <- params[['early_stopping_rounds']]
                    self$nthreads <- params[['nthreads']]
                    self$verbose <- params[['verbose']]
                    self$params <- params
                    self$preds = list()
                    self$params[['nrounds']] <- NULL
                    self$params[['nthreads']] <- NULL
                    self$params[['verbose']] <- NULL
                    self$params[['size']] <- NULL
                    self$params[['train_perc']] <- NULL
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
                    } else if (data_source == 'data') {
                      self$train_data <- this_deng$data_eng(self$train_data)
                    } else {
                      stop('ERROR: data source must be train, test, or data.')
                    }
                  },


                  # Each member of the ensemble has a sample of the 
                  # training data, the proportion specified by "perc"
                  # or percentage.
                  sample_data = function(perc) {
                    res0 <- list()
                    # make the data list
                    
                    if (perc < 1.0) {
                      ## generate random index
                      idx <- sample.int(n = nrow(self$train_data), size = perc*nrow(self$train_data), replace = F)
                      res0[['data']] <- as.matrix(self$train_data[idx,])
                      res0[['label']] <- as.vector(self$label[idx])
                    } else {
                      res0[['data']] <- as.matrix(self$train_data)
                      res0[['label']] <- as.vector(self$label)
                    } 
                    
                    return(res0)
                    
                  },
                  
                  
                  train_models = function(perc) {
                    # for each classifier from 1:size
                    #    train classifier randomly sampling with rate "perc"
                    for (i in 1:self$size) {
                      
                      # sub-sample the data
                      sdat  <- self$sample_data(perc)
                      n_classes <- length(unique(sdat[['label']]))
                      
                      dtrain <- xgb.DMatrix(data=sdat[['data']], 
                                            label = sdat[['label']],
                                            nthread=self$nthreads)
                      
                      if (self$obj_mode != 'final') {
                        self$bstl[[i]] <- xgboost(params=self$params, 
                                                  data=dtrain, 
                                                  nrounds=self$nrounds,
                                                  early_stopping_rounds=self$early_stopping_rounds,
                                                  verbose = self$verbose)
                      } else {
                        # it's multiclass final 
                        self$params[['num_class']] <- n_classes
                        
                        self$bstl[[i]] <- xgboost(params=self$params, 
                                                  data=dtrain, 
                                                  nrounds=self$nrounds,
                                                  early_stopping_rounds=self$early_stopping_rounds,
                                                  verbose = self$verbose)
                        
                      }

                    }
                  },

                  
                  ensemble_combine = function(combine_function) {
                    
                    if (combine_function == 'max') {
                      self$pred_combined <- apply(self$pred_table, 1, max)
                    }
                    else if (combine_function == 'mean') {
                      self$pred_combined <- apply(self$pred_table, 1, mean)
                    }
                    else if (combine_function == 'median') {
                      self$pred_combined <- apply(self$pred_table, 1, median)
                    }
                    else {
                      print('SELECT MAX OR MEAN')
                    }
                  },
                  
                  
                  
                  final_ensemble_combine = function(combine_function) {
                    
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
                  
                  
                  
                  member_predict = function(data, combine_function){
                    # for each member of the ensemble
                    for (i in 1:self$size) {
                      # make a prediction on this data
                      self$preds[[i]] <- predict(self$bstl[[i]], data)
                    }
                    
                    # then group all the predictions together
                    self$pred_table <- do.call(cbind.data.frame, self$preds)
                    colnames(self$pred_table) <- sapply(1:self$size, function(a) paste0('ep',a))
                    
                    # and make a final call or combine the predictions using a function
                    final_combine_function <- ''
                    if (self$name == 'final') { # then we might have have multiclass calls.
                      self$pred_combined <- self$final_ensemble_combine(final_combine_function)
                    } else {
                      # then we combine all the predictions by applying the combine_function
                      self$pred_combined <- self$ensemble_combine(self$combine_function)
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
