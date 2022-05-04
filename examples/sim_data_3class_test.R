
# Example using the autopred (auto-prediction) function.

source('R/recm_obj.R')
source('R/util_fun.R')
source('R/deng_obj.R')
source('R/enbl_obj.R')

# our robust ensemble classifier machine
anne <- Recm$new("Anne")

# xgboost parameters
params <- list(max_depth=6,
               eta=0.1,
               nrounds=5,
               nthreads=4,
               verbose=0)

# this will train the classifier and test it on a small 20% split
anne$autopred(data_file='data/sim_data_3classes_train.csv',
             label_name='label',
             sample_id=NULL,
             data_split=0.80,
             data_mode=c('pairs'), #'quartiles', 'original', 'ranks', 'pairs', 'sigpairs'
             size=7,
             params=params,
             train_perc=0.5,
             combine_function='median')

# metrics on the test split
print("Test split")
anne$final_classification_metrics() %>% print()

# NOW we'll get the test data set up and make predictions
anne$test_data_setup(
  file_name='data/sim_data_3classes_test.csv',
  label_name='label',
  sample_id=NULL)

# make the predictions
anne$predict(anne$test_data, 'median')

# metrics on the test set.
print("New Data")
anne$final_classification_metrics() %>% print()

# return the results
res0 <- anne$results(include_label = TRUE)
print(head(res0))

# confusion matrix
print(table(res0$BestCalls, res0$Label))

# and get the importance of features in each ensemble member
anne$importance()

# plot the ROC curves for each class
ensemble_rocs(anne)

# From sim_data.R, where this test data were generated from:
# Label 1 pairs are 2,7 and 1,9
# Label 2 pairs are 3,6 and 4,5
# Label 3 pairs are 11,12 and 8,10
#These pairs should be recovered in the importance() table.

