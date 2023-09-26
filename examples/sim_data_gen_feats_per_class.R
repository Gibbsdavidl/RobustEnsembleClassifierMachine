

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

labels <- sample(1:nclasses, size = nsamples, replace = T)
datamat <- mat.or.vec(nr = nsamples, nc = nfeatures)
colnames(datamat) <- feature_names
rownames(datamat) <- sample_names

for(ci in 1:nclasses) {
  for (j in seq.int(1,nfeatures,2)) {
    upclass <- which(sapply(featuremap, function(a) j %in% a))
    if (ci == upclass) {
      # this is a predictive feature
      thisdat <- gencol0(ci, labels, 3)
      datamat[thisdat[[1]],j]     <- thisdat[[2]]
      datamat[thisdat[[1]],(j+1)] <- thisdat[[3]]
    } else {
      # not a predictive feature
      thisdat <- gencol0(ci, labels, 0)
      datamat[thisdat[[1]],j]     <- thisdat[[2]]
      datamat[thisdat[[1]],(j+1)] <- thisdat[[3]]
    }
  }
}



