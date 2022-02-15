
# Example using the autopred (auto-prediction) function.

source('R/recm_obj.R')

ann <- Recm$new("Ann")


ann$autopred(data_file='data/sim_data_3classes_train.csv',
             sep=',',
             label_name='label',
             drop_list = NULL,
             data_split=0.80,
             data_mode=c('original'), #'quartiles', 'original', 'ranks', 'pairs', 'sigpairs'
             signatures=NULL,
             size=6,
             max_depth=3,
             eta=0.3,
             nrounds=5,
             nthreads=4,
             objective="binary:logistic",
             train_perc=0.5,
             combine_function='median')

metrics <- ann$final_classification_metrics()
print(metrics)

# NOW we'll read in the training data
# https://www.kaggle.com/merishnasuwal/breast-cancer-prediction-dataset #
ann$test_data_setup(
  file_name='data/sim_data_3classes_test.csv',
  sep=',', 
  label_name='label', 
  drop_list=NULL)

ann$predict_final(ann$test_data, 'median')

metrics <- ann$final_classification_metrics()
print(metrics)

