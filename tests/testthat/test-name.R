test_that("object names works", {
  ann <- Recm$new("Ann")
  expect_equal(ann$name, "Ann")
})
