

# generate simualted data

N <- 200  # number of observations

classes <- 3  # number of classes to predict, labels

vars <- 12

df <- c()
labels <- c()

# inside loop?
var_levels1 <- (rnorm(vars, mean = 20, sd = 10))
var_levels2 <- (rnorm(vars, mean = 20, sd = 10))
var_levels3 <- (rnorm(vars, mean = 20, sd = 10))

for (i in 1:N) {
  
  ci <- sample.int(n=3, size = 1)
  
  labels <- c(labels, ci)
  
  if (ci == 1) {
    
    samp_levels1 <- sapply(var_levels1, function(vi) rnorm(1, vi, 2)) # sampling the values for L1
    samp_levels1[7] <- samp_levels1[2] + 10
    samp_levels1[9] <- samp_levels1[1] + 10
    
    samp_levels1[3] <- samp_levels1[6] - 10
    samp_levels1[4] <- samp_levels1[5] - 10
    
    samp_levels1[11] <- samp_levels1[12] - 10
    samp_levels1[8] <- samp_levels1[10] - 10    
    
    df <- rbind(df, data.frame(matrix(samp_levels1, nr = 1, nc = vars)))
    
  } else if (ci == 2) {
    
    samp_levels2 <- sapply(var_levels2, function(vi) rnorm(1, vi, 2)) # sampling the values for L1
    samp_levels2[7] <- samp_levels2[2] - 10
    samp_levels2[9] <- samp_levels2[1] - 10
    
    samp_levels2[3] <- samp_levels2[6] + 10
    samp_levels2[4] <- samp_levels2[5] + 10
    
    samp_levels2[11] <- samp_levels2[12] - 10
    samp_levels2[8] <- samp_levels2[10] - 10    
    
    df <- rbind(df, data.frame(matrix(samp_levels2, nc = vars)))
    
  } else if (ci == 3) {
    
    samp_levels3 <- sapply(var_levels3, function(vi) rnorm(1, vi, 2)) # sampling the values for L1
    
    samp_levels3[7] <- samp_levels3[2] - 10
    samp_levels3[9] <- samp_levels3[1] - 10
    
    samp_levels3[3] <- samp_levels3[6] - 10
    samp_levels3[4] <- samp_levels3[5] - 10
    
    samp_levels3[11] <- samp_levels3[12] + 10
    samp_levels3[8] <- samp_levels3[10] + 10    
    
    df <- rbind(df, data.frame(matrix(samp_levels3, nc = vars)))
  }
  
}

df["label"] <- labels

write.csv(df, file='data/sim_data_3classes_train.csv', quote = F, row.names = F)



df <- c()
labels <- c()

# inside loop?
var_levels1 <- (rnorm(vars, mean = 20, sd = 10))
var_levels2 <- (rnorm(vars, mean = 20, sd = 10))
var_levels3 <- (rnorm(vars, mean = 20, sd = 10))

for (i in 1:N) {
  
  ci <- sample.int(n=3, size = 1)
  
  labels <- c(labels, ci)
  
  if (ci == 1) {
    
    samp_levels1 <- sapply(var_levels1, function(vi) rnorm(1, vi, 2)) # sampling the values for L1
    samp_levels1[7] <- samp_levels1[2] + 10
    samp_levels1[9] <- samp_levels1[1] + 10
    
    samp_levels1[3] <- samp_levels1[6] - 10
    samp_levels1[4] <- samp_levels1[5] - 10
    
    samp_levels1[11] <- samp_levels1[12] - 10
    samp_levels1[8] <- samp_levels1[10] - 10    
    
    df <- rbind(df, data.frame(matrix(samp_levels1, nr = 1, nc = vars)))
    
  } else if (ci == 2) {
    
    samp_levels2 <- sapply(var_levels2, function(vi) rnorm(1, vi, 2)) # sampling the values for L1
    samp_levels2[7] <- samp_levels2[2] - 10
    samp_levels2[9] <- samp_levels2[1] - 10
    
    samp_levels2[3] <- samp_levels2[6] + 10
    samp_levels2[4] <- samp_levels2[5] + 10
    
    samp_levels2[11] <- samp_levels2[12] - 10
    samp_levels2[8] <- samp_levels2[10] - 10    
    
    df <- rbind(df, data.frame(matrix(samp_levels2, nc = vars)))
    
  } else if (ci == 3) {
    
    samp_levels3 <- sapply(var_levels3, function(vi) rnorm(1, vi, 2)) # sampling the values for L1
    
    samp_levels3[7] <- samp_levels3[2] - 10
    samp_levels3[9] <- samp_levels3[1] - 10
    
    samp_levels3[3] <- samp_levels3[6] - 10
    samp_levels3[4] <- samp_levels3[5] - 10
    
    samp_levels3[11] <- samp_levels3[12] + 10
    samp_levels3[8] <- samp_levels3[10] + 10    
    
    df <- rbind(df, data.frame(matrix(samp_levels3, nc = vars)))
  }  
}

df["label"] <- labels

write.csv(df, file='data/sim_data_3classes_test.csv', quote = F, row.names = F)



