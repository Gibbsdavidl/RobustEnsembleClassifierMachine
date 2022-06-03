test_that("object names works", {
  anne <- Robencla$new("Ann")
  expect_equal(anne$name, "Ann")
})
