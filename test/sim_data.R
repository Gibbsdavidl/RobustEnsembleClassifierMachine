

# Generate simulated data for pairs prediction.
# 
# Here, each variable is normally distributed, but 
# within classes, there are pairs relationships.
# A classifier trained on the original data will do
# poorly on the test data, but if trained on pairs,
# the can perform adequately.


N <- 200  # number of observations

classes <- 3  # number of classes to predict, labels

vars <- 12  # number of variables to simulate


############### TRAIN DATA ###########################

df <- c()  # resulting data.frame
labels <- c() # and labels for each row

for (i in 1:N) {
  
  ci <- sample.int(n=3, size = 1)
  ci <- paste0('label_',ci,collapse = '')
  
  labels <- c(labels, ci)
  
  var_values <- rnorm(vars, 5, 2) # sampling the values for L1
  
  if (ci == 'label_1') {
    var_values[7] <- var_values[2] + .1
    var_values[9] <- var_values[1] + .1
    df <- rbind(df, data.frame(matrix(var_values, nr = 1, nc = vars)))
    
  } else if (ci == 'label_2') {
    var_values[3] <- var_values[6] + .1
    var_values[4] <- var_values[5] + .1
    df <- rbind(df, data.frame(matrix(var_values, nc = vars)))
    
  } else if (ci == 'label_3') {
    var_values[11] <- var_values[12] + .1
    var_values[8] <- var_values[10] + .1    
    df <- rbind(df, data.frame(matrix(var_values, nc = vars)))
  }
  
}

df["label"] <- labels

write.csv(df, file='data/sim_data_3classes_train.csv', quote = F, row.names = F)

############ TEST DATA #################

df <- c()
labels <- c()

for (i in 1:N) {
  
  ci <- sample.int(n=3, size = 1)
  ci <- paste0('label_',ci,collapse = '')
  
  labels <- c(labels, ci)
  
  var_values <- rnorm(vars, 5, 2) # sampling the values for L1
  
  if (ci == 'label_1') {
    var_values[7] <- var_values[2] + .1
    var_values[9] <- var_values[1] + .1
    df <- rbind(df, data.frame(matrix(var_values, nr = 1, nc = vars)))
    
  } else if (ci == 'label_2') {
    var_values[3] <- var_values[6] + .1
    var_values[4] <- var_values[5] + .1
    df <- rbind(df, data.frame(matrix(var_values, nc = vars)))
    
  } else if (ci == 'label_3') {
    var_values[11] <- var_values[12] + .1
    var_values[8] <- var_values[10] + .1    
    df <- rbind(df, data.frame(matrix(var_values, nc = vars)))
  }
  
}

df["label"] <- labels

write.csv(df, file='data/sim_data_3classes_test.csv', quote = F, row.names = F)



