
# Example using the autopred (auto-prediction) function.

source('R/recm_obj.R')

ann <- Recm$new("Ann")

# this will train the classifier and test it on a small split
ann$autopred(data_file='data/sim_data_3classes_train.csv',
             sep=',',
             label_name='label',
             drop_list = NULL,
             data_split=0.70,
             data_mode=c('pairs'), #'quartiles', 'original', 'ranks', 'pairs', 'sigpairs'
             signatures=NULL,
             size=7,
             max_depth=3,
             eta=0.3,
             nrounds=9,
             nthreads=4,
             objective="binary:logistic",
             train_perc=0.5,
             combine_function='median')

# metrics on the test split
ann$final_classification_metrics() %>% print()

# NOW we'll get the test data set up and make predictions
ann$test_data_setup(
  file_name='data/sim_data_3classes_test.csv',
  label_name='label')

# make the predictions
ann$predict(ann$test_data, 'median')

# metrics on the test set.
ann$final_classification_metrics() %>% print()

