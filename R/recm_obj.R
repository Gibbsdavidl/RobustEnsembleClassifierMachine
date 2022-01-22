
# R6 object

library(R6)
library(data.table)

source('R/enbl_obj.R')

#' Recm  Robust ensemble classifier machine (Recm)
#' 
#' An object that holds data and an ensemble of classifiers
#' The ensemble is composed of XGBoost classifiers trained on binarized labels.
#'
#' @examples
#' # New object
#' ann <- Recm$new("Ann")
#' 
#' @export
Recm <- R6Class("Recm",
                  public = list(
                    
                    #' @field name the object's name
                    name = NULL,
                    
                    #' @field mode the data processing mode, noproc, pairs, 
                    data_mode = NULL,
                    
                    #' @field signatures lists of variables, must be in data, that will be used together, and compared to other signatures
                    signatures = NULL,
                    
                    #' @field file_name the data file
                    file_name = NULL,
                    
                    #' @field data the data.table used to train and test
                    data = NULL,
                    
                    #' @field the data column used as label, the target
                    label = NULL,
                    
                    #' @field data_split numeric value indicating percent data to make into training data
                    data_split = NULL,
                    
                    #' @field train_data the data.table used to train the ensemble
                    train_data = NULL,
                    
                    #' @field train_label the vector used as the training label
                    train_label = NULL,
                    
                    #'@field test_data the data.table used as test data
                    test_data = NULL,
                    
                    #' @field test_label the vector used as test data labels
                    test_label = NULL,
                    
                    #' @field  unique_labels the unique set of labels
                    unique_labels = NULL,
                    
                    #' @field the ensemble of predictors
                    ensbl = list(),
                    
                    #' @field the table of predictions, collecting from the ensbl list of predictors
                    pred_table = NULL,
                    
                    #' @description Create a new `Recm` object.
                    #' @param name The object is named.
                    #' @return A new `recm` object.
                    initialize = function(name = NA) {
                      self$name <- name
                      self$greet()
                    },
                    
                    
                    #' @description
                    #' Creates a printable representation of the object.
                    #' @return A character string representing the object.
                    greet = function() {
                      cat(paste0("Hello, my name is ", self$name, ".\n"))
                    },
                    
                    
                    #' @description Reads the data file.
                    #' @param file_name The name of the file.
                    #' @param sep The separting character ',' or '\t'
                    read_data = function(file_name, sep, header) {
                      self$file_name <- file_name
                      self$data <- data.table::fread(file=file_name, sep=sep, header=T)
                    },
                    
                    
                    # data engineering
                    data_eng = function() {
                      print(paste0("self mode: ", self$data_mode))
                      
                      rankdat <- NULL
                      pairdat <- NULL
                      newdat <- data.table()
                      
                      if ('noproc' %in% self$data_mode) {
                        print("*** NO DATA PROCESSING ***")
                        # do not process data
                        return()
                      }
                      
                      if ('rank' %in% self$data_mode) {
                        cols <- colnames(ann$data)
                        rankdat <- ann$data[ , (cols) := lapply(.SD, "rank"), .SDcols = cols]
                        newdat <- cbind(newdat, rankdat)
                      } 
                      
                      if ('pairs' %in% self$data_mode) {
                        print("*** PROCESSING DATA: PAIRS ***")
                        # if mode includes 'pairs' then we need to make var-pairs
                        newcol_names <- c()
                        newcol_dat <- list()
                        if (is.null(rankdat)) {
                          cols <- colnames(ann$data)
                          # we are going to compare ranked data, to make it fair comparisons.
                          rankdat = ann$data[ , (cols) := lapply(.SD, "rank"), .SDcols = cols]
                        }
                        for (ci in 1:(length(cols)-1)) {
                          for (cj in (ci+1):length(cols)) {
                            res0 <- as.numeric(rankdat[,.SD,.SDcols=ci] > rankdat[,.SD,.SDcols=cj])
                            this_new_col <- paste0(cols[ci],'_X_', cols[cj])
                            newcol_names <- c(newcol_names, this_new_col)
                            newcol_dat[[this_new_col]] <- res0
                          }
                        }
                        pairdat <- data.table(data.frame(newcol_dat))
                        newdat <- cbind(newdat, pairdat)
                      }

                      # save the final processed data table.
                      ann$data <- newdat
                    },
                    

                    #' @description Does some setup processing on the data file, drop columns, split data into train and test, and identify the label column.
                    #' @param label_name string, the column name indicating the target label
                    #' @param drop_list a vector of strings indicating what columns to drop
                    #' @param data_split numeric value, the percent of data to use in training 
                    data_setup = function(data_mode=NULL, 
                                          signatures=NULL, 
                                          label_name=NULL, 
                                          drop_list=NULL, 
                                          data_split=NULL){
                      # INITIAL setup
                      self$data_mode <- data_mode
                      self$signatures <- signatures
                      self$label <- sapply(self$data[[label_name]], as.character)
                      self$data[, (label_name):=NULL]
                      self$data[, (drop_list):=NULL]
                      # DATA ENGINEERING
                      self$data_eng()
                      # to split the data into training and test components
                      n <- nrow(self$data)
                      idx <- sample.int(n = n, size=data_split*n)
                      jdx <- setdiff( (1:n), idx)
                      # then create the data sets
                      self$train_data <- self$data[idx,]
                      self$train_label <- self$label[idx]
                      self$test_data <- self$data[jdx,]
                      self$test_label <- self$label[jdx]
                      # and record the unique categories in labels 
                      self$unique_labels <- unique(self$train_label)
                    },
                    
                    
                    # takes the label, and returns a vector of 0s and 1s
                    binarize_label = function(label, x) {
                      if (is.numeric(x)) {
                        x <- as.character(x)
                      }
                      new_label <- ifelse(label == x, yes=1, no=0)
                      return(new_label)
                    },
                    
                    
                    #' @description 
                    #' Builds list of ensembles of XGBoost object, each classifying one binary label.
                    #' @param mode character vector, what types of data modalities to make. possible: pairs, quartiles, set-pairs
                    #' @param size numeric, number of classifiers
                    #' @param label string, the label vector of each data example
                    #' @param max_depth numeric, the depth of the tree in XGBoost
                    #' @param eta numeric, the eta param of XGBoost, speed of learning
                    #' @param nrounds numeric, the number of training rounds
                    #' @param nthreads numeric, the number of threads to use in processing
                    #' @param objective string, binary:logistic, see xgboost docs
                    #'
                    #' @details A list of classifiers, each trained on a random sample of the training data.
                    #'
                    #' @return A ensemble object is added to the list of objects in recm$enbl.
                    #'
                    #' @examples
                    #' ann$build_ensemble('e1', c('pairs'), 21, data = ann$train_data, label = 4,
                    #' max_depth = 7, eta = 0.3, nrounds = 5,
                    #' nthreads = 4, objective = "binary:logistic")
                    #'
                    build_label_ensemble = function(
                                              size, 
                                              max_depth, 
                                              eta, 
                                              nrounds,
                                              nthreads, 
                                              objective) {
                      
                      # for each category
                      for (li in self$unique_labels) {
                        # first create the binarized label
                        bin_label <- self$binarize_label(label=self$train_label, x=li)
                        # then create the classifier object
                        self$ensbl[[li]] <- Ensbl$new(li, 
                                                      'ensemble',
                                                      size, 
                                                      self$train_data,
                                                      bin_label, 
                                                      max_depth, 
                                                      eta, 
                                                      nrounds,
                                                      nthreads,
                                                      objective)
                        
                      }
                    },
                    
                    
                    
                    build_pred_table = function() {
                      final_train_data <- list()
                      for (li in self$unique_labels) {
                        final_train_data[[li]] <- ann$ensbl[[li]]$pred_combined
                      }
                      
                      self$pred_table <- do.call(cbind.data.frame, final_train_data)
                    },
                    
                    
                    
                    remap_multiclass_labels = function(label) {
                      mapper <- list() # first we construct a mapper function
                      idx <- 0
                      for (li in self$unique_labels) { 
                        mapper[[li]] <- idx
                        idx <- idx+1
                      }
                      new_labels <- sapply(label, function(a) mapper[[a]])
                      return(new_labels)
                    },
                    
                    
                    
                    build_final_ensemble = function(
                                                size, 
                                                max_depth, 
                                                eta, 
                                                nrounds,
                                                nthreads, 
                                                objective) {
                      # here the ensemble will be trained on the prior predictions
                      # for each category, get the predictions
                      # turn that into a dataframe / matrix
                      self$build_pred_table()
                      remapped_label <- self$remap_multiclass_labels(self$train_label)
                      #print(head(self$train_label))
                      #print(head(remapped_label))
                      # train a XGBoost that takes multiple labels.
                      self$ensbl[["final"]] <- Ensbl$new("final",
                                                      "final",
                                                      size, 
                                                      self$pred_table,
                                                      remapped_label, 
                                                      max_depth, 
                                                      eta, 
                                                      nrounds,
                                                      nthreads,
                                                      objective
                                                    )

                    },
                    
                    
                    
                    train_models = function(perc) {
                      for (li in self$unique_labels) {
                        self$ensbl[[li]]$train_models(perc)
                      }
                    },
                    
                    
                    
                    train_final = function(perc) {
                      self$ensbl[['final']]$train_models(perc)
                    },
                    
                    
                    
                    predict_ensemble = function(data, combine_function) {
                      if (all(class(data)[1] == 'matrix') == FALSE) {
                        data <- as.matrix(data)
                      }
                      for (li in self$unique_labels) {
                        self$ensbl[[li]]$ensemble_predict(data, combine_function)
                      }
                    },
                    
                    
                    # predict final uses predictions from predict_ensemble
                    predict_final = function(data, combine_function) {
                      self$predict_ensemble(data, combine_function)
                      self$build_pred_table()
                      # then we should have a new pred_table from the data
                      pred_matrix <- as.matrix(self$pred_table)
                      self$ensbl[['final']]$ensemble_predict(pred_matrix, combine_function)
                    },
                    
                    
                    
                    print_error = function(label, root, threshold) {
                      if (all(class(label) == 'numeric') == FALSE) {
                        label <- as.numeric(label)
                      }
                      new_label <- self$binarize_label(label, root)
                      self$ensbl[[root]]$print_error(new_label, threshold)
                    },
                    
                    
                    
                    print_final_error = function(test_label, threshold) {
                      remapped_label <- self$remap_multiclass_labels(test_label)
                      self$ensbl[['final']]$print_error(remapped_label, threshold)
                    },

                    
                    autopred = function(data_file=NULL,
                                        sep=NULL,
                                        label_name=NULL,
                                        drop_list=NULL,
                                        data_split=NULL,
                                        data_mode=NULL,
                                        signatures=NULL,
                                        size=NULL,
                                        max_depth=NULL,
                                        eta=NULL,
                                        nrounds=NULL,
                                        nthreads=NULL,
                                        objective=NULL,
                                        train_perc=NULL,
                                        combine_function=NULL
                                      ) {
                      
                      # first read the data
                      ann$read_data(data_file, sep)
                      
                      # perform the data set up
                      ann$data_setup(data_mode=data_mode,
                                     signatures=signatures,
                                     label_name=label_name, 
                                     drop_list=drop_list, 
                                     data_split=data_split)
                      
                      # build the initial set of predictors
                      ann$build_label_ensemble(size=size, 
                                               max_depth = max_depth, 
                                               eta = eta, 
                                               nrounds = nrounds,
                                               nthreads = nthreads, 
                                               objective = objective)
                      
                      # and train them using a random selection of data
                      ann$train_models(train_perc)
                      
                      # then make a prediction on the training data
                      ann$predict_ensemble(self$train_data, combine_function = combine_function)
                      
                      # build the output predictor
                      ann$build_final_ensemble(size=size, 
                                               max_depth=max_depth, 
                                               eta=eta, 
                                               nrounds=nrounds,
                                               nthreads = nthreads, 
                                               objective = 'multi:softmax')

                      # and use the earlier training predictions to train the output                      
                      ann$train_final(train_perc)
                      
                      # and finally, make a prediction on some training data.
                      ann$predict_final(self$test_data, combine_function)
                      
                    }
                    
                  ) # end public
      )


