
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
#' Set$new(1:5)
#'
#' # Set of multiple types
#' Set$new("a", 5, Set$new(1))
#'
#' # Each Set has properties and traits
#' s <- Set$new(1, 2, 3)
#' s$traits
#' s$properties
#'
#' # Elements cannot be duplicated
#' Set$new(2, 2) == Set$new(2)
#'
#' # Ordering does not matter
#' Set$new(1, 2) == Set$new(2, 1)
#' @export
Ensbl <- R6Class("Ensbl",
                public = list(
                  bstl = list(),  # booster list
                  name = NULL,
                  mode = NULL,
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
                                        mode,
                                        size, 
                                        data, 
                                        label, 
                                        max_depth, 
                                        eta, 
                                        nrounds,
                                        nthreads, 
                                        objective) {
                    self$name <- name
                    self$mode <- mode
                    self$size <- size
                    self$data = data 
                    self$label = label 
                    self$max_depth = max_depth
                    self$eta = eta
                    self$nrounds = nrounds
                    self$nthreads = nthreads 
                    self$objective = objective
                    self$preds = list()
                  },
                  
                  
                  print = function(...) {
                    paste0('ensemble: ', self$name ) 
                  },
                  
                  
                  
                  sample_data = function(perc) {
                    ## generate random index
                    idx <- sample.int(n = nrow(self$data), size = perc*nrow(self$data), replace = F)
                    res0 <- list()
                    res0[['data']] <- as.matrix(self$data[idx,])
                    res0[['label']] <- as.vector(self$label[idx])
                    res0
                  },
                  
                  
                  
                  data_eng = function(xdat) {
                    # xdat is going to be a data.table

                    return(xdat)
                    
                  },
                  
                  
                  
                  train_models = function(perc) {
                    # for each classifier from 1:size
                    #    train classifier randomly sampling with rate "perc"
                    for (i in 1:self$size) {
                      
                      # sub-sample the data
                      sdat <- self$sample_data(perc)
                      
                      # realized data engineering should take place on the sample
                      pdat <- self$data_eng(sdat)
                      
                      # need to set stopping point?
                      self$bstl[[i]] <- xgboost(data = pdat[['data']], 
                                                label = pdat[['label']], 
                                                max_depth = self$max_depth, 
                                                eta = self$eta, 
                                                nrounds = self$nrounds,
                                                nthread = self$nthreads, 
                                                objective = self$objective, 
                                                early_stopping_rounds=2)
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
