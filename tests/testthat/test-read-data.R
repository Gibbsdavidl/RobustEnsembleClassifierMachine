
# test that the object can read data

test_that("object can read data", {
  anne <- Recm$new("Anne")
  anne$read_data(file_name='testdata/bcp_train_data.csv', ',', header=T)
  dat <- read.csv('testdata/bcp_train_data.csv')
  # read.csv replaces spaces with dots
  colnames(dat) <- gsub("\\.", "_", colnames(dat))
  # 399  11
  expect_equal(nrow(anne$data), nrow(dat))
  expect_equal(ncol(anne$data), ncol(dat))
  expect_equal(colnames(anne$data), colnames(dat))
})


test_that("object can read data to training", {
  anne <- Recm$new("Anne")
  anne$read_train_data(file_name='testdata/bcp_train_data.csv', ',', header=T)
  dat <- read.csv('testdata/bcp_train_data.csv')
  colnames(dat) <- gsub("\\.", "_", colnames(dat))
  # 399  11
  expect_equal(nrow(anne$train_data), nrow(dat))
  expect_equal(ncol(anne$train_data), ncol(dat))
  expect_equal(colnames(anne$train_data), colnames(dat))
})


test_that("object can read data to testing", {
  anne <- Recm$new("Anne")
  anne$read_test_data(file_name='testdata/bcp_test_data.csv', ',', header=T)
  dat <- read.csv('testdata/bcp_test_data.csv')
  # read.csv replaces spaces with dots
  colnames(dat) <- gsub("\\.", "_", colnames(dat))
  # 399  11
  expect_equal(nrow(anne$test_data), nrow(dat))
  expect_equal(ncol(anne$test_data), ncol(dat))
  expect_equal(colnames(anne$test_data), colnames(dat))
})


test_that("data is split correctly", {
  
  anne <- Recm$new("Anne")

  anne$data_setup(file_name='testdata/bcp_train_data.csv', 
    sep=',', 
    data_mode='original', 
    signatures=NULL, 
    label_name='Class', 
    drop_list='Sample code number', 
    data_split=0.6)
  
  # reading in the train data
  dat <- read.csv('testdata/bcp_train_data.csv')
  # read.csv replaces spaces with dots
  colnames(dat) <- gsub("\\.", "_", colnames(dat))
  dat <- dat[, 2:10]
  # 399  11
  # make the index
  idx <- 1:nrow(dat)
  idx1 <- idx[1:(0.6*nrow(dat))]
  idx2 <- setdiff(idx, idx1)
  # split the data
  train_dat <- dat[idx1,]
  test_dat  <- dat[idx2,]
  #     
  expect_equal(ncol(anne$train_data), ncol(train_dat))
  expect_equal(nrow(anne$test_data), nrow(test_dat))
  expect_equal(ncol(anne$test_data), ncol(test_dat))
  expect_equal(colnames(anne$test_data), colnames(test_dat))
  
})


test_that("data_mode, label_name, and drop_list errors are caught", {

  anne <- Recm$new("Anne")
  params <- list(max_depth=6,
                 eta=0.1,
                 nrounds=5,
                 nthreads=4,
                 verbose=0)
  
  expect_error(
    anne$autopred(data_file='testdata/bcp_train_data.csv',
                 label_name='Class',
                 drop_list = c('Sample code number'),
                 data_split=0.60,
                 data_mode=c('___sigpairs___', 'quartiles'), # c('original', 'ranks', 'pairs'), # 'sigpairs'
                 signatures=NULL,
                 size=8,
                 params=params,
                 train_perc=0.5,
                 combine_function='median'),
    'data_mode, wrong value'
  )  
  
  
  expect_error(
    anne$data_setup(file_name='testdata/bcp_train_data.csv', 
                    sep=',', 
                    data_mode='__original__',  ### incorrect mode
                    signatures=NULL, 
                    label_name='Class', 
                    drop_list='Sample code number', 
                    data_split=0.6),
    'data_mode, wrong value'
  )  

    expect_error(
    anne$data_setup(file_name='testdata/bcp_train_data.csv', 
                    sep=',', 
                    data_mode='original', 
                    signatures=NULL, 
                    label_name='A_Class', ### wrong label name
                    drop_list='Sample code number', 
                    data_split=0.6),
    'Make sure the label name matches one of the columns!'
    )  
  
  anne <- Recm$new("Anne")
  expect_error(
    anne$data_setup(file_name='testdata/bcp_train_data.csv', 
                    sep=',', 
                    data_mode='original', 
                    signatures=NULL, 
                    label_name='Class', 
                    drop_list='___Sample code number____', ### doesn't exist
                    data_split=0.6),
    'Make sure the drop_list contains column names found in the data!'
  )  
  
})


test_that("Label NULL values are caught", {
  anne <- Recm$new("Anne")
  expect_error(
    anne$data_setup(file_name='testdata/bcp_train_data.csv', 
                    sep=',', 
                    data_mode='original', 
                    signatures=NULL, 
                    label_name=NULL, 
                    drop_list='Sample code number', 
                    data_split=0.6),
    "Make sure label_name is not null!"
  )    
})



