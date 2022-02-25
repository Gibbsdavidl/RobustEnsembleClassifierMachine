

### Test example 1

### Building, training, and making predictions with 
### the recm object.

### Here, the code is used step by step.  The workflow
### will be bundled into a single call and demonstrated 
### in test2.R

# library(RobustEnsembleClassifierMachine)

source('R/recm_obj.R')
source('R/util_fun.R')
source('R/deng_obj.R')
source('R/enbl_obj.R')

# The object's name is Ann, OK?
ann <- Recm$new("Ann")

# https://www.kaggle.com/merishnasuwal/breast-cancer-prediction-dataset #
ann$train_data_setup(
  file_name = 'data/bcp_train_data.csv', 
  sep=',',
  data_mode='pairs',
  label_name='Class',
  drop_list=c('Sample code number'))

params <- list(max_depth=6,
               eta=0.1,
               nrounds=5,
               nthreads=4,
               verbose=0)

# building the first layer of predictors, each a binary prediction
# on one factor in the target labels.
ann$build_label_ensemble(size=5, 
                         params=params)$
    train_models(0.6)$
    ensemble_predict(ann$train_data, 'median')

final_params <- params
final_params[['objective']] <- 'multi:softmax'
final_params[['eval_metric']] <- 'mlogloss'

# then we build the output layer
ann$build_final_ensemble(size=5, final_params)$train_final(0.6)

# NOW we'll read in the training data
# https://www.kaggle.com/merishnasuwal/breast-cancer-prediction-dataset #
ann$test_data_setup(
  file_name='data/bcp_test_data.csv', 
  label_name='Class', 
  drop_list=c('Sample code number'))

ann$predict(ann$test_data, 'median')


metrics <- ann$final_classification_metrics()
print(metrics)



