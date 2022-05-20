
# Utility functions


plot_roc <- function(recm_obj, i) {
  # for each ensemble object, we'll have a plot with multiple lines
  # related to predictions on test data.
  rocList <- list() # each member of teh ensemble will have a rocit
  
  # each predictor in the ensemble will get a line in the ROC
  for (j in 1:(recm_obj$ensbl[[i]]$size)){
  
    if (i <= length(recm_obj$unique_labels)) {
      label <- recm_obj$binarize_label(recm_obj$test_label, recm_obj$unique_labels[i])
    } else {
      label <- recm_obj$test_label
    }
    
    if (length(unique(label)) > 2) {
      print("Too many classes. ROC plots are for binary classes only.")
      return()
    }
    
    scorej = recm_obj$ensbl[[i]]$pred_table[,j]
    
    rocList[[j]] <- rocit(score = scorej, class = label)
  }
  
  # plot the first, then the second, then add the legend
  plot(rocList[[1]], col = c(2,"gray50"), 
       legend = FALSE, YIndex = FALSE, title="asdf")
  
  for (k in 2:(recm_obj$ensbl[[i]]$size)){
    lines(rocList[[k]]$TPR ~ rocList[[k]]$FPR, 
          col = 2, lwd = 2)
  }
  if (i <= length(recm_obj$unique_labels)) {
    legend("bottomright", paste0("Class: ",recm_obj$unique_labels[i]), lty = c(1))
  } else {
    legend("bottomright", paste0("Class: Final"), lty = c(1))
  }
}



# This function will plot the ROC from
# the ensemble supporting each class. 
ensemble_rocs <- function(recm_obj){
  
  N <- length(recm_obj$unique_labels)
  
  par(mfcol=c(N,1))
  
  for (j in 1:length(recm_obj$unique_labels)){
      plot_roc(recm_obj, j)
  }
  
}



