

# Example using the autopred (auto-prediction) function.

source('R/recm_obj.R')

ann <- Recm$new("Ann")

sigs = list(Sig1=c('Uniformity.of.Cell.Shape','Uniformity.of.Cell.Shape', 'Marginal.Adhesion'), 
            Sig2=c('Bare.Nuclei', 'Normal.Nucleoli', 'Single.Epithelial.Cell.Size'),
            Sig3=c('Bland.Chromatin', 'Mitoses'))

ann$autopred(data_file='data/Breast Cancer Prediction.csv',
             sep=',',
             label_name='Class',
             drop_list = c('Sample code number'),
             data_split=0.60,
             data_mode='pairs', #c('rank', 'pairs'), #, 'sigpairs'), #c('noproc'),
             signatures=sigs,
             size=8,
             max_depth=6,
             eta=0.5,
             nrounds=7,
             nthreads=4,
             objective="binary:logistic",
             train_perc=0.5,
             combine_function='median')

ann$print_final_error(ann$test_label, 0.5) 