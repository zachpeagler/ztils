test_that("outliers will leave", {
  set.seed(123)
  tdat <- data.frame(x = rlnorm(100, 0, 5))
  notdat <- no_outliers(tdat, x)
  expect_equal(nrow(notdat), 79)
})

test_that("no outliers remain", {
  set.seed(123)
  tdat <- data.frame(x = rlnorm(100, 0, 5))
  notdat <- no_outliers(tdat, x)
  q <- quantile(tdat$x, probs = c(.25, .75), na.rm = TRUE)
  iqr <- IQR(tdat$x)
  otdat <- subset(tdat, tdat$x < (q[1] - 1.5 * iqr) |
                    tdat$x > (q[2] + 1.5 * iqr))
  p <- data.frame(otdat$x %in% notdat$x)
  if (TRUE %in% p) {
    p_out <- TRUE
  } else {
    p_out <- FALSE
  }
  expect_equal(p_out, FALSE)
})
