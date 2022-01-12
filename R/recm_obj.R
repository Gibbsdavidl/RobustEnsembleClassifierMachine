
# R6 object

library(R6)
library(data.table)

source('R/enbl_obj.R')

#' @name Recm
#' @title Robust ensemble classifier machine (Recm)
#' @description An object that holds data and an ensemble of classifiers
#'
#' @details
#' The ensemble is composed of XGBoost classifiers trained on binarized labels.
#'
#' @examples
#' # New object
#' ann <- Recm$new("Ann")
#' @export
Recm <- R6Class("Recm",
                  public = list(
                    name = NULL,
                    file_name = NULL,
                    data = NULL,
                    data_split = NULL,
                    train_data = NULL,
                    train_label = NULL,
                    test_data = NULL,
                    test_label = NULL,
                    label = NULL,
                    ensbl = NULL,
                    
                    
                    #' @description Create a new `Recm` object.
                    #' @param name The object is named.
                    #' @return A new `Set` object.
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
                    
                    
                    
                    read_data = function(filename, sep, header, label_name) {
                      self$file_name <- filename
                      self$data <- data.table::fread(file=filename, sep=sep, header=header)
                    },
                    
                    
                    
                    data_setup = function(label_name, drop_list, data_split){
                      self$label <- self$data[[label_name]]
                      self$data[, (label_name):=NULL]
                      self$data[, (drop_list):=NULL]
                      # to split the data into training and test components
                      n <- nrow(self$data)
                      idx <- sample.int(n = n, size=data_split*n)
                      jdx <- setdiff( (1:n), idx)
                      
                      self$train_data <- self$data[idx,]
                      self$train_label <- self$label[idx]
                      self$test_data <- self$data[jdx,]
                      self$test_label <- self$label[jdx]
                    },
                    
                    
                    
                    binarize_label = function(label, x) {
                      new_label <- ifelse(label == x, yes=1, no=0)
                      return(new_label)
                    },
                    
                    
                    
                    #' @description Tests to see if \code{x} is contained in the Set.
                    #'
                    #' @param x any. Object or vector of objects to test.
                    #' @param all logical. If `FALSE` tests each `x` separately. Otherwise returns `TRUE` only if all `x` pass test.
                    #' @param bound ignored, added for consistency.
                    #'
                    #' @details \code{x} can be of any type, including a Set itself. \code{x} should be a tuple if
                    #' checking to see if it lies within a set of dimension greater than one. To test for multiple \code{x}
                    #' at the same time, then provide these as a list.
                    #'
                    #' If `all = TRUE` then returns `TRUE` if all `x` are contained in the `Set`, otherwise
                    #' returns a vector of logicals.
                    #'
                    #' @return If \code{all} is `TRUE` then returns `TRUE` if all elements of \code{x} are contained in the `Set`, otherwise
                    #' `FALSE.` If \code{all} is `FALSE` then returns a vector of logicals corresponding to each individual
                    #' element of \code{x}.
                    #'
                    #' The infix operator `%inset%` is available to test if `x` is an element in the `Set`,
                    #' see examples.
                    #'
                    #' @examples
                    #' s = Set$new(elements = 1:5)
                    #'
                    #' # Simplest case
                    #' s$contains(4)
                    #' 8 %inset% s
                    #'
                    #' # Test if multiple elements lie in the set
                    #' s$contains(4:6, all = FALSE)
                    #' s$contains(4:6, all = TRUE)
                    #'
                    #' # Check if a tuple lies in a Set of higher dimension
                    #' s2 = s * s
                    #' s2$contains(Tuple$new(2,1))
                    #' c(Tuple$new(2,1), Tuple$new(1,7), 2) %inset% s2
                    build_ensemble = function(name, 
                                              mode,
                                              size, 
                                              data, 
                                              label, 
                                              max_depth, 
                                              eta, 
                                              nrounds,
                                              nthreads, 
                                              objective) {
                      # first create the binarized label
                      bin_label <- self$binarize_label(label=self$train_label, x=label)
                      # then create the classifier object
                      self$ensbl <- Ensbl$new(name, 
                                              mode,
                                              size, 
                                              data, 
                                              bin_label, 
                                              max_depth, 
                                              eta, 
                                              nrounds,
                                              nthreads,
                                              objective)
                    },
                    
                    
                    train_models = function(perc) {
                      self$ensbl$train_models(perc)
                    },
                    
                    
                    
                    predict_ensemble = function(data, combine_function) {
                      if (all(class(data)[1] == 'matrix') == FALSE) {
                        data <- as.matrix(data)
                      }
                      
                      self$ensbl$ensemble_predict(data, combine_function)
                    },
                    
                    
                    print_error = function(label, root, threshold) {
                      if (all(class(label) == 'numeric') == FALSE) {
                        label <- as.numeric(label)
                      }
                      new_label <- self$binarize_label(label, root)
                      self$ensbl$print_error(new_label, threshold)
                    }
                    
                    
                  ) # end public
      )


