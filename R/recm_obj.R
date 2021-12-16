
# R6 object

library(R6)
library(data.table)

source('R/enbl_obj.R')

#' @name Set
#' @title Mathematical Set
#' @description A general Set object for mathematical sets. This also serves as the parent class to
#' intervals, tuples, and fuzzy variants.
#' @family sets
#'
#' @details
#' Mathematical sets can loosely be thought of as a collection of objects of any kind. The Set class
#' is used for sets of finite elements, for infinite sets use [Interval]. These can be
#' expanded for fuzzy logic by using [FuzzySet]s. Elements in a set cannot be duplicated and ordering
#' of elements does not matter, [Tuple]s can be used if duplicates or ordering are required.
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
Recm <- R6Class("Recm",
                  public = list(
                    name = NULL,
                    file_name = NULL,
                    data = NULL,
                    label = NULL,
                    ensbl = NULL,
                    
                    #' @description Create a new `Set` object.
                    #' @param ... `ANY` Elements can be of any class except `list`, as long as there is a unique
                    #' `as.character` coercion method available.
                    #' @param universe Set. Universe that the Set lives in, i.e. elements that could be added to
                    #' the Set. Default is [Universal].
                    #' @param elements list. Alternative constructor that may be more efficient if passing objects
                    #' of multiple classes.
                    #' @param class character. Optional string naming a class that if supplied gives the set the
                    #' `typed` property. All elements will be coerced to this class and therefore there must be
                    #' a coercion method to this class available.
                    #' @return A new `Set` object.
                    initialize = function(name = NA) {
                      self$name <- name
                      self$greet()
                    },
                    
                    #' @description
                    #' Creates a printable representation of the object.
                    #' @param n numeric. Number of elements to display on either side of ellipsis when printing.
                    #' @return A character string representing the object.
                    greet = function() {
                      cat(paste0("Hello, my name is ", self$name, ".\n"))
                    },
                    
                    read_data = function(filename, sep, header) {
                      self$file_name <- filename
                      self$data <- data.table::fread(file=filename, sep=sep, header=header)
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
                                              size, 
                                              data, 
                                              label, 
                                              max_depth, 
                                              eta, 
                                              nrounds,
                                              nthread, 
                                              objective) {
                      self$ensbl <- Ensbl$new(name, 
                                              size, 
                                              data, 
                                              label, 
                                              max_depth, 
                                              eta, 
                                              nrounds,
                                              nthread, 
                                              objective)
                    }
                  ) # end public
      )


