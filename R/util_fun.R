
# Utility functions

# bin data vectors into 4 levels
data_bin_4 <- function(x) {
  p = quantile(x)
  if (any(table(p) > 1)) {
    # then we have duplicate levels
    p <- unique(p)
  }
  as.numeric(cut(x, breaks=p, include.lowest = T))
}