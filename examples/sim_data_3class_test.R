
# Example using the autopred (auto-prediction) function.

library(devtools)

tmp_lib <- "E:/Work/Code/tmp_lib"
dir.create(tmp_lib)

devtools::install_local("E:/Work/Code/robencla/", lib = tmp_lib)

## restart R

## explicitly load the affected packages from the temporary library
tmp_lib <- "E:/Work/Code/tmp_lib"
library(robencla, lib.loc = tmp_lib)


# our robust ensemble classifier machine
anne <- Robencla$new("Anne")

# xgboost parameters
params <- list(max_depth=6,
               eta=0.1,
               nrounds=5,
               nthreads=4,
               verbose=0,
               size=7,
               train_perc=0.6,
               combine_function='median')

# this will train the classifier and test it on a small 20% split
anne$autocv(data_file='data/sim_data_3classes_train.csv',
             label_name='label',
             sample_id=NULL,
             cv_rounds=5,
             data_split=0.80,
             data_mode=c('pairs'), #'quartiles', 'original', 'ranks', 'pairs', 'sigpairs'
             params=params)

# metrics on the test split
print("Across splits")
anne$classification_metrics() %>% print()

