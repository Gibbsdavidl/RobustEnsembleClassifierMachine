
# Utility functions

# can be quickly applied to data.table

# bin data vectors into 4 levels
data_bin_4 <- function(x) {
  p = quantile(x)
  if (any(table(p) > 1)) {
    # then we have duplicate levels
    p <- unique(p)
  }
  as.numeric(cut(x, breaks=p, include.lowest = T))
}


# bin data vectors into 3 levels
data_bin_3 <- function(x) {
  p = quantile(x, probs = c(0, 0.33333, 0.66666, 1))
  if (any(table(p) > 1)) {
    # then we have duplicate levels
    p <- unique(p)
  }
  as.numeric(cut(x, breaks=p, include.lowest = T))
}


# bin data vectors into 2 levels
data_bin_2 <- function(x) {
  p = quantile(x, probs = c(0, 0.5, 1))
  if (any(table(p) > 1)) {
    # then we have duplicate levels
    p <- unique(p)
  }
  as.numeric(cut(x, breaks=p, include.lowest = T))
}



get_list_of_importance_tables <- function(enslb_obj, label) {
  
  ldf <- list()
  for (i in 1:length(anne$ensbl[[label]]$bstl)) {
    ldf[[i]] <- xgb.importance(model = anne$ensbl[[label]]$bstl[[i]])
  }
  return(ldf)
}


unique(unlist(sapply(ldf, function(a) a$Feature)))






