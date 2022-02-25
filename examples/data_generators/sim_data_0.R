

# generate simualted data

N <- 200  # number of observations

classes <- 3  # number of classes to predict, labels

vars <- 3

df <- c()
labels <- c()


for (i in 1:N) {
  
  ci <- sample.int(n=3, size = 1)
  ci <- paste0('label_',ci,collapse = '')
  
  labels <- c(labels, ci)
  
  if (ci == 'label_1') {
    
    samp_levels1 <- sapply(c(5,1,1), function(vi) rnorm(1, vi, 2)) # sampling the values for L1

    df <- rbind(df, data.frame(X1=samp_levels1[1],
                               X2=samp_levels1[2],
                               X3=samp_levels1[3]))
    
  } else if (ci == 'label_2') {
    
    samp_levels1 <- sapply(c(1,5,1), function(vi) rnorm(1, vi, 2)) # sampling the values for L1
    
    df <- rbind(df, data.frame(X1=samp_levels1[1],
                               X2=samp_levels1[2],
                               X3=samp_levels1[3]))
    
  } else if (ci == 'label_3') {
    
    samp_levels1 <- sapply(c(1,1,5), function(vi) rnorm(1, vi, 2)) # sampling the values for L1
    
    df <- rbind(df, data.frame(X1=samp_levels1[1],
                               X2=samp_levels1[2],
                               X3=samp_levels1[3]))
  }
  
}

df["label"] <- labels

write.csv(df, file='data/sim_data_3classes_train0.csv', quote = F, row.names = F)



df <- c()
labels <- c()


for (i in 1:N) {
  
  ci <- sample.int(n=3, size = 1)
  ci <- paste0('label_',ci,collapse = '')
  
  labels <- c(labels, ci)
  
  if (ci == 'label_1') {
    
    samp_levels1 <- sapply(c(5,1,1), function(vi) rnorm(1, vi, 2)) # sampling the values for L1
    
    df <- rbind(df, data.frame(X1=samp_levels1[1],
                               X2=samp_levels1[2],
                               X3=samp_levels1[3]))
    
  } else if (ci == 'label_2') {
    
    samp_levels1 <- sapply(c(1,5,1), function(vi) rnorm(1, vi, 2)) # sampling the values for L1
    
    df <- rbind(df, data.frame(X1=samp_levels1[1],
                               X2=samp_levels1[2],
                               X3=samp_levels1[3]))
    
  } else if (ci == 'label_3') {
    
    samp_levels1 <- sapply(c(1,1,5), function(vi) rnorm(1, vi, 2)) # sampling the values for L1
    
    df <- rbind(df, data.frame(X1=samp_levels1[1],
                               X2=samp_levels1[2],
                               X3=samp_levels1[3]))
  }
  
}

df["label"] <- labels

write.csv(df, file='data/sim_data_3classes_test0.csv', quote = F, row.names = F)



