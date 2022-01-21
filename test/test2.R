

source('R/recm_obj.R')

ann <- Recm$new("Ann")

ann$autopred(data_file='data/Breast Cancer Prediction.csv',
             sep=',',
             label_name='Class',
             drop_list = c('Sample code number'),
             data_split=0.80,
             mode=c('pairs'),
             size=8,
             max_depth=12,
             eta=0.1,
             nrounds=21,
             nthreads=4,
             objective="binary:logistic",
             train_perc=0.75,
             combine_function='median')

ann$print_final_error(ann$test_label, 0.5) 