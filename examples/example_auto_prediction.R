

# Example using the autopred (auto-prediction) function.

#source('R/recm_obj.R')
#source('R/util_fun.R')
#source('R/deng_obj.R')
#source('R/enbl_obj.R')

library(RobustEnsembleClassifierMachine)

anne <- Recm$new("Anne")

# list of signatures to compare
sigs = list(Sig1=c('Uniformity of Cell Shape','Uniformity of Cell Size', 'Marginal Adhesion'), 
            Sig2=c('Bare Nuclei', 'Normal Nucleoli', 'Single Epithelial Cell Size'),
            Sig3=c('Bland Chromatin', 'Mitoses'))

# xgboost parameters
params <- list(max_depth=6,
               eta=0.2,
               nrounds=12,
               nthreads=4,
               verbose=0)

# split the data, train and test
anne$autopred(data_file='data/Breast Cancer Prediction.csv',
             label_name='Class',
             sample_id = 'Sample code number',
             cv_rounds=3,
             data_split=0.60,
             data_mode=c('sigpairs'), # 'quartiles', 'original', 'ranks', 'pairs', 'sigpairs'
             signatures=sigs,
             size=11,
             params=params,
             train_perc=0.3,
             combine_function='median')

# print the test data results table
print(
  head(
    anne$cv_results
    )
  )

# metrics on the test set predictions
print(
  anne$classification_metrics() # uses CV results
  )

# and get the importance of features in each ensemble member
print(
  anne$importance() # uses the last fold trained.
)

# plot the ROC curves for each class
ensemble_rocs(anne) # uses the last fold trained.

print(dim(anne$cv_results))

