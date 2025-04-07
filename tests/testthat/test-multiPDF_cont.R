test_that("lognormal error when negative", {
  set.seed(123)
  t <- data.frame(x = rnorm(10, 0, 1))
  expect_error(multiPDF_cont(t$x))
})
