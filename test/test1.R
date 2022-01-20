

source('R/recm_obj.R')

ann <- Recm$new("Ann")

# https://www.kaggle.com/merishnasuwal/breast-cancer-prediction-dataset #
ann$read_data('data/Breast Cancer Prediction.csv', ',', T)

ann$data_setup(label_name='Class', drop_list=c('Sample code number'), 
               data_split = 0.6)


ann$build_ensemble('e1', c('pairs'), 21, data = ann$train_data, label = 4,
                   max_depth = 7, eta = 0.3, nrounds = 5,
                   nthreads = 4, objective = "binary:logistic")

ann$train_models(0.6)

ann$predict_ensemble(ann$test_data, 'median')

ann$print_error(ann$test_label, 4, 0.5)
