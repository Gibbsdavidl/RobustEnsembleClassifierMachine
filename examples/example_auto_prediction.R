

# Example using the autopred (auto-prediction) function.

library(robencla)

anne <- Robencla$new("Anne")

# list of signatures to compare
sigs = list(Sig1=c('Uniformity of Cell Shape','Uniformity of Cell Size', 'Marginal Adhesion'), 
            Sig2=c('Bare Nuclei', 'Normal Nucleoli', 'Single Epithelial Cell Size'),
            Sig3=c('Bland Chromatin', 'Mitoses'))

# only pair these features
my_pairs <- c('Clump Thickness','Uniformity of Cell Size','Uniformity of Cell Shape','Marginal Adhesion')

# xgboost parameters
params <- list(max_depth=6,
               eta=0.2,
               nrounds=12,
               nthreads=4,
               size=11,
               train_perc=0.3,
               combine_function='median',
               verbose=0)

# split the data, train and test
anne$autocv(data_file='data/Breast Cancer Prediction.csv',
             label_name='Class',
             sample_id = 'Sample code number',
             cv_rounds=3,
             data_split=0.60,
             data_mode=c('pairs', 'sigpairs'), # pairs,sigpairs,quartiles,tertiles,binary,ranks,original
             pair_list=my_pairs,
             signatures=sigs,
             params=params)

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
## IF THE ROC IS UPSIDE DOWN, SET FLIP=T
ensemble_rocs(anne, flip=F) # uses the last fold trained.

print(dim(anne$cv_results))

# plot the ROC curves for each class
#ensemble_rocs(anne, flip=T) # uses the last fold trained.

