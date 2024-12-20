
# Utility functions for plotting results


plot_roc <- function(mod, label) {
  # for each ensemble object, we'll have a plot with multiple lines
  # related to predictions on test data.
  rocList <- list() # each member of teh ensemble will have a rocit
  i <- which(names(mod$ensbl) == label)
  bin_label <- ifelse(mod$test_label == label, yes=1, no=0)  

  # each predictor in the ensemble will get a line in the ROC
  for (j in 1:(mod$ensbl[[i]]$size)){

    # score for ensemble member j, for label i
    scorej = mod$ensbl[[i]]$pred_table[,j]
    
    rocList[[j]] <- rocit(score = scorej, class = bin_label)
  }
  
  # plot the first, then the second, then add the legend
  plot(rocList[[1]], col = c(2,"gray50"), 
       legend = FALSE, YIndex = FALSE, title=label)
  
  for (k in 2:(mod$ensbl[[i]]$size)){
    lines(rocList[[k]]$TPR ~ rocList[[k]]$FPR, 
          col = 2, lwd = 2)
  }
  
  if (i <= length(mod$unique_labels)) {
    legend("bottomright", paste0("Class: ",label), lty = c(1))
  } else {
    legend("bottomright", paste0("Class: Final"), lty = c(1))
  }
}



# This function will plot the ROC from
# the ensemble supporting each class. 

## IF THE ROC IS UPSIDE DOWN, SET FLIP=T
ensemble_rocs <- function(mod){
  
  N <- length(mod$unique_labels)
  
  par(mfcol=c(N,1))
  
  for (label in mod$unique_labels){
      plot_roc(mod, label)
  }
  
}


# This function plots a heatmap of prediction
# probabilities for each example (x-axis), 
# across ensemble members (y-axis)
# It show the last thing predicted, on the test,
# so if you want to see it on training data, then
# run training data through the predict function.
plot_pred_heatmap <- function(mod, 
                              label='label_1',
                              include_label=T, # which label group to look at
                              cluster=TRUE,  
                              show_rownames=F) {
  require(pheatmap)
  
  if (include_label) {
    df <- mod$ensbl[[label]]$pred_table
    res_df <- mod$results(include_label = T)
    # first two columns are rowname and best call
    annotdf = res_df[,c(2, ncol(res_df))]
    num_df <- as.matrix(df)
    rownames(num_df) <- rownames(annotdf)
    pheatmap(num_df, annotation_row = annotdf, 
             cluster_rows = cluster, show_rownames=show_rownames)
  } else {
    df <- mod$ensbl[[label]]$pred_table
    # first two columns are rowname and best call
    num_df <- as.matrix(df)
    rownames(num_df) <- rownames(df)
    pheatmap(num_df, #annotation_row = annotdf, 
             cluster_rows = cluster, show_rownames=show_rownames)
  }
  
  }


# This function plots a heatmap of prediction
# probabilities for each example (x-axis), 
# across ensemble members (y-axis)
# It show the last thing predicted, on the test,
# so if you want to see it on training data, then
# run training data through the predict function.
plot_pred_final <- function(mod, 
                            include_label=T,
                            annotation=TRUE, 
                            cluster=TRUE,
                            show_rownames=F) {
                              
  require(pheatmap)
  
  if (include_label) {
    df <- na.omit( 
      mod$results(include_label = include_label)
    )
    # first two columns are rowname and best call
    a <- 3
    b <- ncol(df)-1
    annotdf = df[,c(2, ncol(df))]
    num_df <- as.matrix(df[,a:b])
    rownames(num_df) <- rownames(df)
    pheatmap(num_df, annotation_row = annotdf, 
             cluster_rows = cluster, show_rownames=show_rownames)
  } else {
    df <- mod$results(include_label = include_label)
    # first two columns are rowname and best call
    a <- 3
    b <- ncol(df)
    num_df <- as.matrix(df[,a:b])
    rownames(num_df) <- rownames(df)
    pheatmap(num_df, #annotation_row = annotdf, 
             cluster_rows = cluster, show_rownames=show_rownames)
  }
  

}  

