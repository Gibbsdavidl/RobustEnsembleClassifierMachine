
# enbl ensemble object

# library(xgboost)


#' @name Metrics
#' @title An ensemble of xgboosts
#' @description Each member of the ensemble is an xgboost, and contains a random sample of the data.
#'
#' @details
#' The ensbl object contains a list of xgboost objects
#'
#'
Metrics <- R6Class("Metrics",
                   
                   public = list(
                     labels = NULL,  # the label vector 
                     calls = NULL,   # the vector of discrete calls
                     #probs = NULL,   # the vector of probabilties (or scores)
                     
                     initialize = function(labels=NULL,
                                           calls=NULL #probs=NULL
                                           ){
                       self$labels <- labels
                       self$calls  <- calls
                       #self$probs  <- probs
                     },
                     
                     
                     # total percent correct from all calls
                     accuracy = function(labels, calls) {
                       m <- sum(labels == calls)
                       return(m/length(labels))
                     },
                     
                     # precision
                     # what proportion of predicted positives are truly positive
                     precision = function(cmdf, i) {
                       called_pos <- cmdf %>% dplyr::filter(calls==i) %>% pull('Freq')
                       true_pos <- cmdf %>% dplyr::filter(labels==i & calls == i) %>% pull('Freq')
                       return(true_pos / sum(called_pos))
                     },
                     
                     # sensitivity
                     # what proportion of true positives are called positive
                     sensitivity = function(cmdf, i) {
                       true_labels <- cmdf %>% dplyr::filter(labels==i) %>% pull('Freq')
                       true_pos <- cmdf %>% dplyr::filter(labels==i & calls == i) %>% pull('Freq')
                       return(true_pos / sum(true_labels))
                     },
                     
                     # specificity
                     # what proportion of true negatives are called negative
                     specificity = function(cmdf, i) {
                       false_labels <- cmdf %>% dplyr::filter(labels!=i) %>% pull('Freq')
                       true_neg <- cmdf %>% dplyr::filter(labels!=i & calls != i) %>% pull('Freq')
                       return(sum(true_neg) / sum(false_labels))
                     },
                     
                     
                     
                     #' @description 
                     #' Returns a table of classification metrics, one row per label.
                     #' @param these_labels vector, the classification labels.
                     #' @param these_calls vector, the predicted calls.
                     #' @param use_cv_results boolean, use the cross validation results or internal test set
                     #'
                     #' @details Returns various metrics associated with machine learning performance.
                     #'
                     #' @return A table of classification metrics for each label and overall.
                     #'
                     #'
                     compute_metrics = function(these_labels=NULL, these_calls=NULL) {
                       
                       # there are instances where some classes are not returned
                       # in the BestCalls, those can cause numeric(0) in sens, spec, etc.
                       isEmpty <- function(x) {
                         return(identical(x, numeric(0)))
                       }
                       
                       fixMissing <- function(x) {
                         sapply(x, function(a) if(isEmpty(a)){0}else{a})
                       }
                       
                       if ( (!is.null(these_calls)) && (!is.null(these_labels)) ) {
                         labels <- these_labels
                         calls  <- these_calls
                       } else {
                         labels <- self$labels
                         calls <- self$calls
                       }
                       
                       # then build the multi-class confusion matrix
                       confusion_matrix <- table( labels, calls )
                       
                       # labels of the confusion matrix
                       cm_labels <- rownames(confusion_matrix)
                       
                       # and we'll transform that into a data.frame
                       cmdf <- as.data.frame(confusion_matrix, stringsAsFactors = F)
                       
                       # accuracy
                       acc <- self$accuracy(labels, calls)
                       
                       # first compute precision
                       prec <- fixMissing(sapply(cm_labels, function(a) self$precision(cmdf, a)))
                       
                       # then specificity
                       spec <- fixMissing(sapply(cm_labels, function(a) self$specificity(cmdf, a)))
                       
                       # then sensitivity or recall
                       sens <- fixMissing(sapply(cm_labels, function(a) self$sensitivity(cmdf, a)))
                       
                       # then F1
                       if (is.numeric(sens) & is.numeric(prec)) {
                         f1 <-(2*sens*prec) / (sens+prec+0.0000000001)
                       } else {
                         f1 <- 0
                       }
                       
                       metrics_table <- data.frame(Label=cm_labels,
                                             Accuracy=acc,
                                             Sensitivity=sens,
                                             Specificity=spec,
                                             Precision=prec,
                                             F1=f1,
                                             stringsAsFactors = F)
                       
                       avg_row <- data.frame(Label="Average",
                                             Accuracy=mean(metrics_table[,2]),
                                             Sensitivity=mean(metrics_table[,3]),
                                             Specificity=mean(metrics_table[,4]),
                                             Precision=mean(metrics_table[,5]),
                                             F1=mean(metrics_table[,6]), 
                                             row.names = 'Average',
                                             stringsAsFactors = F)
                       
                       metrics_table <- rbind(metrics_table, avg_row)
                       
                       return(metrics_table)
                       
                     }
                   ) # end public
)
                     