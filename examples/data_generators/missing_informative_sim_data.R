

# Generate simulated data for pairs prediction.
# 
# Here, each variable is normally distributed, but 
# within classes, there are pairs relationships.
# A classifier trained on the original data will do
# poorly on the test data, but if trained on pairs,
# the can perform adequately.


N <- 500  # number of observations

classes <- 3  # number of classes to predict, labels

vars <- 24  # number of variables to simulate


############### TRAIN DATA ###########################

df <- c()  # resulting data.frame
labels <- c() # and labels for each row

for (i in 1:N) {
  
  ci <- sample.int(n=3, size = 1)
  ci <- paste0('label_',ci,collapse = '')
  
  labels <- c(labels, ci)
  
  var_values <- rnorm(vars, 5, 2) # sampling the values for L1
  
  if (ci == 'label_1') {
    var_values[1] <- var_values[2] + .1
    var_values[3] <- var_values[4] + .1
    var_values[5] <- var_values[6] + .1
    df <- rbind(df, data.frame(matrix(var_values, nr = 1, nc = vars)))
    
  } else if (ci == 'label_2') {
    var_values[7] <- var_values[8] + .1
    var_values[9] <- var_values[10] + .1
    var_values[11] <- var_values[12] + .1
    df <- rbind(df, data.frame(matrix(var_values, nc = vars)))
    
  } else if (ci == 'label_3') {
    var_values[13] <- var_values[14] + .1
    var_values[15] <- var_values[16] + .1    
    var_values[17] <- var_values[18] + .1
    df <- rbind(df, data.frame(matrix(var_values, nc = vars)))
  }
  
}

df["label"] <- labels

# label 1 will always have 1&2, but only half and half with have the others, etc
label_1_idx <- which(df$label == 'label_1')
label_1_sel <- sample(label_1_idx, size = length(label_1_idx)/3, replace = F)
label_1_non <- sample(label_1_idx, size = length(label_1_idx)/3, replace = F) #setdiff(label_1_idx, label_1_sel)
df[label_1_sel, 'X3'] <- rnorm(n=length(label_1_sel))
df[label_1_non, 'X5'] <- rnorm(n=length(label_1_non))

# label 2 will always have 7&8, but only half and half with have the others, etc
label_2_idx <- which(df$label == 'label_2')
label_2_sel <- sample(label_2_idx, size = length(label_2_idx)/3, replace = F)
label_2_non <- sample(label_2_idx, size = length(label_2_idx)/3, replace = F) # setdiff(label_2_idx, label_2_sel)
df[label_2_sel, 'X9'] <- rnorm(n=length(label_2_sel))
df[label_2_non, 'X11'] <- rnorm(n=length(label_2_non))

# label 1 will always have 1&2, but only half and half with have the others, etc
label_3_idx <- which(df$label == 'label_3')
label_3_sel <- sample(label_3_idx, size = length(label_3_idx)/3, replace = F)
label_3_non <- sample(label_3_idx, size = length(label_3_idx)/3, replace = F) #setdiff(label_3_idx, label_3_sel)
df[label_3_sel, 'X15'] <- rnorm(n=length(label_3_sel))
df[label_3_non, 'X17'] <- rnorm(n=length(label_3_non))

write.csv(df, file='examples/data/missing_informative_train_data.csv', quote = F, row.names = F)

############ TEST DATA #################

df <- c()  # resulting data.frame
labels <- c() # and labels for each row

for (i in 1:N) {
  
  ci <- sample.int(n=3, size = 1)
  ci <- paste0('label_',ci,collapse = '')
  
  labels <- c(labels, ci)
  
  var_values <- rnorm(vars, 5, 2) # sampling the values for L1
  
  if (ci == 'label_1') {
    #var_values[1] <- var_values[2] + .1
    var_values[3] <- var_values[4] + .1
    var_values[5] <- var_values[6] + .1
    df <- rbind(df, data.frame(matrix(var_values, nr = 1, nc = vars)))
    
  } else if (ci == 'label_2') {
    #var_values[7] <- var_values[8] + .1
    var_values[9] <- var_values[10] + .1
    var_values[11] <- var_values[12] + .1
    df <- rbind(df, data.frame(matrix(var_values, nc = vars)))
    
  } else if (ci == 'label_3') {
    #var_values[13] <- var_values[14] + .1
    var_values[15] <- var_values[16] + .1    
    var_values[17] <- var_values[18] + .1
    df <- rbind(df, data.frame(matrix(var_values, nc = vars)))
  }
  
}

df["label"] <- labels

write.csv(df, file='examples/data/missing_informative_test_data.csv', quote = F, row.names = F)


