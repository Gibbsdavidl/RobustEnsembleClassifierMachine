

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
ann$data_setup(file_name='data/Breast Cancer Prediction.csv',
               sep=',',
               data_mode='pairs',
               label_name='Class', 
               drop_list=c('Sample code number'), 
               data_split = 0.6)


# building the first layer of predictors, each a binary prediction
# on one factor in the target labels.
ann$build_label_ensemble(size=5, 
                   max_depth = 7, eta = 0.3, nrounds = 5,
                   nthreads = 4, objective = "binary:logistic")

# and training and predicting on the training data
ann$train_models(0.6)
ann$predict_ensemble(ann$train_data, 'median')


# then we build the output layer
ann$build_final_ensemble(size=5, 
                         max_depth=7, eta=0.3, nrounds=5,
                         nthreads = 4, objective = 'multi:softmax')

# and train it on the output of the previous layer
ann$train_final(0.6)
ann$predict_final(ann$test_data, 'median')

# now we're ready to make some predictions on the test data
print("The '4' label predictor error:")
ann$print_error(ann$test_label, '4', 0.5)

print("\nThe '2' label predictor error:")
ann$print_error(ann$test_label, '2', 0.5)

print("\nThe final output predictor error:")
ann$print_final_error(ann$test_label, 0.5)

