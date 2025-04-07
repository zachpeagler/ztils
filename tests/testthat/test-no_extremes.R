test_that("extremes will leave", {
  set.seed(123)
  tdat <- data.frame(x = rlnorm(100, 0, 5))
  netdat <- no_extremes(tdat, x)
  expect_equal(nrow(netdat), 82)
})

test_that("no extremes remain", {
  set.seed(123)
  tdat <- data.frame(x = rlnorm(100, 0, 5))
  netdat <- no_extremes(tdat, x)
  Q <- quantile(tdat$x, probs=c(.25, .75), na.rm=TRUE)
  iqr <- IQR(tdat$x)
  otdat <- subset(tdat, tdat$x < (Q[1] - 3 * iqr) |
                    tdat$x > (Q[2] + 3 * iqr))
  p <- data.frame(otdat$x %in% netdat$x)
  if (TRUE %in% p) {
    p_out <- TRUE
  } else {
    p_out <- FALSE
  }
  expect_equal(p_out, FALSE)
})
