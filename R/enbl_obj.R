
# ensemble object

library(xgboost)


#' @name Ensbl
#' @title An ensemble of xgboosts
#' @description Each member of the ensemble is an xgboost, and contains a random sample of the data.
#'
#' @details
#' The ensbl object contains a list of xgboost objects
#'
#' @examples
#' # Set of integers
#' 
#' ann$build_label_ensemble(c('pairs'), size=5,
#'   max_depth = 7, eta = 0.3, nrounds = 5,
#'   nthreads = 4, objective = "binary:logistic")
#' 
#' ann$train_models(0.6)
#' 
#' ann$predict_ensemble(ann$train_data, 'median')
#'
Ensbl <- R6Class("Ensbl",
                public = list(
                  bstl = list(),  # booster list
                  name = NULL,
                  obj_mode = NULL,
                  size  = NULL,
                  perc = NULL,
                  data = NULL, 
                  label = NULL, 
                  max_depth = NULL, 
                  eta = NULL, 
                  nrounds = NULL,
                  nthreads = NULL, 
                  objective = NULL,
                  preds = NULL,
                  pred_table = NULL,
                  pred_combined = NULL,
                  
                  initialize = function(name,
                                        obj_mode,
                                        size, 
                                        data, 
                                        label, 
                                        max_depth, 
                                        eta, 
                                        nrounds,
                                        nthreads, 
                                        objective) {
                    self$name <- name
                    self$obj_mode <- obj_mode
                    self$size <- size
                    self$data <- data 
                    self$label <- label 
                    self$max_depth <- max_depth
                    self$eta <- eta
                    self$nrounds <- nrounds
                    self$nthreads <- nthreads 
                    self$objective <- objective
                    self$preds = list()
                    
                  },
                  
                  
                  print = function(...) {
                    paste0('ensemble: ', self$name ) 
                  },
                  
                  
                  # Each member of the ensemble has a sample of the 
                  # training data, the proportion specified by "perc"
                  # or percentage.
                  sample_data = function(perc) {
                    res0 <- list()
                    # make the data list
                    if (perc < 1.0) {
                      ## generate random index
                      idx <- sample.int(n = nrow(self$data), size = perc*nrow(self$data), replace = F)
                      res0[['data']] <- as.matrix(self$data[idx,])
                      res0[['label']] <- as.vector(self$label[idx])
                    } else {
                      res0[['data']] <- as.matrix(self$data)
                      res0[['label']] <- as.vector(self$label)
                    }                   
                    res0
                  },
                  
                  
                  
                  train_models = function(perc) {
                    # for each classifier from 1:size
                    #    train classifier randomly sampling with rate "perc"
                    for (i in 1:self$size) {
                      
                      # sub-sample the data
                      sdat <- self$sample_data(perc)
                      
                      n_classes <- length(unique(sdat[['label']]))
                      
                      if (self$obj_mode != 'final') {
                        self$bstl[[i]] <- xgboost(data = sdat[['data']], 
                                                  label = sdat[['label']], 
                                                  max_depth = self$max_depth, 
                                                  eta = self$eta, 
                                                  nrounds = self$nrounds,
                                                  nthread = self$nthreads, 
                                                  objective = self$objective, 
                                                  eval_metric='logloss', #rmse',  # mlogloss
                                                  early_stopping_rounds=2,
                                                  verbose = 0)
                      } else {
                        # it's multiclass final 
                        self$bstl[[i]] <- xgboost(data = sdat[['data']], 
                                                  label = sdat[['label']], 
                                                  max_depth = self$max_depth, 
                                                  eta = self$eta, 
                                                  nrounds = self$nrounds,
                                                  nthread = self$nthreads, 
                                                  objective = self$objective, 
                                                  eval_metric='mlogloss',  # mlogloss rmse
                                                  early_stopping_rounds=2,
                                                  num_class=n_classes,
                                                  verbose=0)
                        
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
                  
                  
                  
                  ensemble_predict = function(data, combine_function){
                    for (i in 1:self$size) {
                      self$preds[[i]] <- predict(self$bstl[[i]], data)
                    }
                    
                    self$pred_table <- do.call(cbind.data.frame, self$preds)
                    colnames(self$pred_table) <- sapply(1:self$size, function(a) paste0('ep',a))
                    
                    # then we combine all the predictions by applying the combine_function
                    self$pred_combined <- self$ensemble_combine(combine_function)
                    
                  },
                  
                  
                  
                  print_error = function(label, threshold) {
                    
                    for (i in 1:self$size) {
                      err <- as.numeric(sum(as.integer(self$preds[[i]] > threshold) != label)) / length(label)
                      print(paste0('ep', i, ' prediction error: ', err))
                    }
                    
                    err <- as.numeric(sum(as.integer(self$pred_combined > threshold) != label)) / length(label)
                    print(paste0('combined prediction error: ', err))
                  }
                  
                  
                  
                ) # end public
)
