

### Test example 1

### Building, training, and making predictions with 
### the recm object.

### Here, the code is used step by step.  The workflow
### will be bundled into a single call and demonstrated 
### in test2.R

# library(RobustEnsembleClassifierMachine)
source('R/recm_obj.R')

# The object's name is Ann, OK?
ann <- Recm$new("Ann")

# https://www.kaggle.com/merishnasuwal/breast-cancer-prediction-dataset #
ann$train_data_setup(
  file_name = 'data/bcp_train_data.csv', 
  sep=',', 
  data_mode='pairs',
  label_name='Class', 
  drop_list=c('Sample code number'))


# building the first layer of predictors, each a binary prediction
# on one factor in the target labels.
ann$build_label_ensemble(size=5, 
                         max_depth = 7, 
                         eta = 0.3, 
                         nrounds = 5,
                         nthreads = 4, 
                         objective = "binary:logistic")$
    train_models(0.6)$
    predict_ensemble(ann$train_data, 'median')$
    # then we build the output layer
    build_final_ensemble(size=5, 
                         max_depth=7, 
                         eta=0.3, 
                         nrounds=5,
                         nthreads = 4, 
                         objective = 'multi:softmax')$
    train_final(0.6)

# and train it on the output of the previous layer
#ann$train_final(0.6)

# NOW we'll read in the training data
# https://www.kaggle.com/merishnasuwal/breast-cancer-prediction-dataset #
ann$test_data_setup(
  file_name='data/bcp_test_data.csv', 
  sep=',', 
  label_name='Class', 
  drop_list=c('Sample code number'))

ann$predict_final(ann$test_data, 'median')


metrics <- ann$final_classification_metrics(ann$test_label)
print(metrics)



