test_that("zero variance columns caught", {
  anne <- Robencla$new("Ann")
  
  expect_error(
    anne$data_setup(file_name='testdata/bcp_train_data_zero_var.csv', 
                    sep=',', 
                    data_mode='original',
                    label_name='Class',
                    drop_list='Sample code number',
                    data_split=0.6)
    )
})
