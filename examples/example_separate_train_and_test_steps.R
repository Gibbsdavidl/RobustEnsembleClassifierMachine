

### Test example 1

### Building, training, and making predictions with 
### the recm object.

### Here, the code is used step by step.  The workflow
### will be bundled into a single call and demonstrated 
### in test2.R

library(robencla)

# The object's name is Ann, OK?
anne <- Robencla$new("Anne")

# first we read in some data, which is processed into robust features
# https://www.kaggle.com/merishnasuwal/breast-cancer-prediction-dataset #
anne$train_data_setup(
  file_name = 'data/bcp_train_data.csv', 
  data_mode=c('pairs'),  #'quartiles', 'original', 'ranks', 'pairs', 'sigpairs'
  label_name='Class',
  sample_id='Sample code number'
)

# then we select our xgboost params
params <- list(max_depth=6,
               eta=0.1,
               nrounds=5,
               nthreads=4,
               verbose=0)

# building the first layer of predictors, each a binary prediction
# on one factor in the target labels.
# training and making predictions on the training data
anne$build_label_ensemble(size=5, params=params)$
    train_models(0.8)$
    ensemble_predict(anne$train_data, 'median')

# setting some final layer xgboost params
final_params <- params
final_params[['objective']] <- 'multi:softmax'
final_params[['eval_metric']] <- 'mlogloss'

# then we build the output layer, trained on the predictions of the first layer
anne$build_final_ensemble(size=5, final_params)$train_final(0.8)

# NOW we'll read in the test data
# https://www.kaggle.com/merishnasuwal/breast-cancer-prediction-dataset #
anne$test_data_setup(
   file_name='data/bcp_test_data.csv', 
   label_name='Class', 
   sample_id='Sample code number'
   # and make predictions on the test data
)$predict(anne$test_data, 'median')

# return the results
res0 <- anne$results(include_label = TRUE)
print(head(res0))

# confusion matrix
print(table(res0$BestCalls, res0$Label))

# and check out how we did.
# metrics on the test set predictions
print(
  anne$classification_metrics(use_cv_results=FALSE) # No CV results
)
# and get the importance of features in each ensemble member
print(anne$importance())

# plot the ROC curves for each class
## IF THE ROC IS UPSIDE DOWN, SET FLIP=T
ensemble_rocs(anne, flip=F) # uses the last fold trained.
