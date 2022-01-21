

### Test example 1

### Building, training, and making predictions with 
### the recm object.

### Here, the code is used step by step.  The workflow
### will be bundled into a single call and demonstrated 
### in test2.R


source('R/recm_obj.R')

ann <- Recm$new("Ann")

# https://www.kaggle.com/merishnasuwal/breast-cancer-prediction-dataset #
ann$read_data('data/Breast Cancer Prediction.csv', ',', T)
ann$data_setup(label_name='Class', 
               drop_list=c('Sample code number'), 
               data_split = 0.6)



ann$build_label_ensemble(c('pairs'), size=5, 
                   max_depth = 7, eta = 0.3, nrounds = 5,
                   nthreads = 4, objective = "binary:logistic")
ann$train_models(0.6)
ann$predict_ensemble(ann$train_data, 'median')



ann$build_final_ensemble(mode='final', size=5, 
                         max_depth=7, eta=0.3, nrounds=5,
                         nthreads = 4, objective = 'multi:softmax')
ann$train_final(0.6)

ann$predict_final(ann$test_data, 'median')


ann$print_error(ann$test_label, '4', 0.5)
ann$print_error(ann$test_label, '2', 0.5)

ann$print_final_error(ann$test_label, 0.5)
