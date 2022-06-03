

# robencla the rob(ust) en(semble) cla(ssifier)

# Transforms data into robust forms and trains an ensemble of XGboost classifiers.

```
devtools::install_github("gibbsdavidl/robencla")

library(robencla)

# Our classifier object named Anne.  Hi Anne!
anne <- Recm$new("Anne")

# Defining the list of signatures to compute signature-pairs, must be composed of column names
sigs = list(Sig1=c('Uniformity of Cell Shape','Uniformity of Cell Size', 'Marginal Adhesion'), 
            Sig2=c('Bare Nuclei', 'Normal Nucleoli', 'Single Epithelial Cell Size'),
            Sig3=c('Bland Chromatin', 'Mitoses'))
            
# Now, create a list of features, and pairs will only be created from that list.
pair_list <- c('Uniformity of Cell Shape','Uniformity of Cell Size', 'Marginal Adhesion')

# XGBoost parameters to pass to each sub-classifier in the ensembles
params <- list(max_depth=6,
               eta=0.2,
               nrounds=12,
               nthreads=4,
               verbose=0)

# split the data, train and test
anne$autopred(
  # The data to use for training
  data_file='data/Breast Cancer Prediction.csv',
  
  # The name of the column containing the training label
  label_name='Class',
  
  # The column containing the sample ID
  sample_id = 'Sample code number',
  
  # The proportion of samples to use for training, remainder used in test
  data_split=0.60,  # train and test (1-train%) splits.

  # The number of cross validation folds to predict on, ignores data_split if >1.
  cv_rounds=1,
            
  # The data transformation mode, set using  pairs,sigpairs,quartiles,tertiles,binarize,ranks,original
  data_mode=c('sigpairs','pairs'), 
  
  # If using signature pairs, then the list defining the signatures, must be composed of column names
  signatures=sigs,
  
  # Use only these features for the pairs
  pair_list=pairlist,
  
  # Size of the ensemble, per binary prediction 
  size=11,
  
  # The XGBoost parameters
  params=params,
  
  # The percentage of data used to train each ensemble member.
  train_perc=0.3,
  
  # How the ensemble should be combined. Only median currently.
  combine_function='median'
  )



# Now we can print the top of the results table.
# If using cross validation, the table will have an 
# entry for each sample, otherwise only for the test set.
print(
  head(
    anne$cv_results()
    )
  )

# Prediction metrics on the test set predictions
print(
  anne$classification_metrics()
  )

# Get the importance of features in each ensemble member
# these are found by combining the importance measures across the ensemble
print(
  anne$importance()
)

# Plot the ROC curves for each class
ensemble_rocs(anne)

```

Let me know if something's not working!
  david.gibbs@systemsbiology.org
  gibbsdavidl@gmail.com
#
