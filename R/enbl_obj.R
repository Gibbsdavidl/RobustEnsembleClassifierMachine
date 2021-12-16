
# ensemble object

library(xgboost)

Ensbl <- R6Class("Ensbl",
                public = list(
                  bstl = list(),
                  name = NULL,
                  size  = NULL,
                  data = NULL, 
                  label = NULL, 
                  max_depth = NULL, 
                  eta = NULL, 
                  nrounds = NULL,
                  nthread = NULL, 
                  objective = NULL,
                  
                  initialize = function(name, 
                                        size, 
                                        data, 
                                        label, 
                                        max_depth, 
                                        eta, 
                                        nrounds,
                                        nthread, 
                                        objective) {
                    self$name <- name
                    self$size <- size
                    self$data = data 
                    self$label = label 
                    self$max_depth = max_depth
                    self$eta = eta
                    self$nrounds = nrounds
                    self$nthread = nthread 
                    self$objective = objective
                  },
                  
                  print = function(...) {
                    paste0('ensemble: ',name ) 
                  },
                  
                  sample_data = function(perc) {
                    ## generate random index
                    res0 <- list()
                    res0[['data']] <- as.matrix(self$data)
                    res0[['label']] <- as.vector(self$label)
                    res0
                  },
                  
                  train_models = function(perc) {
                    # for each classifier from 1:size
                    #    train classifier randomly sampling with rate "perc"
                    for (i in 1:size) {
                      sdat <- sample_data(perc)
                      self$bstl[[i]] <- xgboost(data = sdat[['data']], 
                                                label = sdat[['label']], 
                                                max_depth = self$max_depth, 
                                                eta = self$eta, 
                                                nrounds = self$nrounds,
                                                nthread = self$nthreads, 
                                                objective = self$objective)
                    }
                  }
                )
)
