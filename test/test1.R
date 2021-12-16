


source('R/recm_obj.R')

ann <- Recm$new("Ann")

ann$read_data('data/Breast Cancer Prediction.csv', ',', T)

ann$build_ensemble('e1', 10, data = ann$data, label = c(), max_depth = 2, eta = 1, nrounds = 2,
                   nthread = 2, objective = "binary:logistic")

ann
