

### Testing feature sets per class

# each class has a set of features that will be predictive
# the features are paired, so that one of the pair will always
# have a higher value.
# 
# 

nclasses <- 3
featsper <- 20
nsamples <- 200
nfeatures <- nclasses*featsper

labels <- sample(1:nclasses, size = nsamples, replace = T)
labels <- as.character(labels)

feature_names <- sapply(1:nfeatures, function(x){
  paste0("feat_",paste0(sample(1:9,size=4),collapse = ''))
})

sample_names <- sapply(1:nsamples, function(x){
  paste0("samp_",paste0(sample(1:9,size=4),collapse = ''))
})

pair_list <- lapply(0:(nclasses-1), function(x){
  print(x*featsper+1)
  print(x*featsper + featsper)
  feature_names[(x*featsper+1):(x*featsper+featsper)]
})

names(pair_list) = c('1','2','3')

featuremap <- lapply(0:(nclasses-1), function(x){
  c( (x*featsper+1):(x*featsper + featsper) )
})


gencol0 <- function(label, labels, dir) {
  idx <- which(label == labels)
  n <- length(idx)
  list(
    idx,
    rnorm(n, mean=0+dir),
    rnorm(n, mean=0)
  )
}


makeData <- function(nclasses, nsamples, nfeatures, labels,
                     feature_names, sample_names, 
                     offset1, offset2, uptrack, numpredfeat) {
  
  datmat <- mat.or.vec(nr = nsamples, nc = nfeatures)
  colnames(datmat) <- feature_names
  rownames(datmat) <- sample_names
  
  if (is.null(uptrack)) {
    uptrack <- sample(c(0,1), size=nfeatures, replace = T)
  }
  
  for(ci in 1:nclasses) {
    predcounter <- numpredfeat
    for (j in seq.int(1, nfeatures ,2)) {
      upclass <- which(sapply(featuremap, function(a) j %in% a))
      if (ci == upclass && predcounter > 0) {
        # this is a predictive feature
        thisdat <- gencol0(ci, labels, offset1)
        # subtrack 1 from the informative features allowed
        predcounter <- predcounter-1
        if (uptrack[j] > 0) {
          datmat[thisdat[[1]],j]     <- thisdat[[2]]
          datmat[thisdat[[1]],(j+1)] <- thisdat[[3]]
        } else {
          datmat[thisdat[[1]],j]     <- thisdat[[3]]
          datmat[thisdat[[1]],(j+1)] <- thisdat[[2]]
        }
      } else {
        # not a predictive feature
        thisdat <- gencol0(ci, labels, offset2)
        datmat[thisdat[[1]],j]     <- thisdat[[2]]
        datmat[thisdat[[1]],(j+1)] <- thisdat[[3]]
      }
    }
  }
  datmat <- as.data.frame(datmat)
  datmat$Label   <- labels
  datmat$Barcode <- sample_names
  return(list(datmat,uptrack))  
}


