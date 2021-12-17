


source('R/recm_obj.R')

ann <- Recm$new("Ann")

ann$read_data('data/Breast Cancer Prediction.csv', ',', T)

ann$data_setup(label_name='Class', drop_list=c('Sample code number'))

ann$binarize_label(2) 

ann$build_ensemble('e1', c('pairs'), 20, data = ann$data, label = ann$label, 
                   max_depth = 3, eta = 1, nrounds = 50,
                   nthreads = 4, objective = "binary:logistic")

ann$train_models(0.5)

ann$predict_ensemble(ann$data, ann$label, 'median', 0.5)

