#' Multiple Cumulative Distribution Functions for Continuous Variables
#' @description
#' This function gets the cumulative distribution function for
#' selected distributions against a continuous, non-negative input variable.
#' Possible distributions include "normal",
#' "lognormal", "gamma", "exponential", "cauchy", "t", "weibull", "logistic",
#' and "all".
#'
#' @param var The variable of which to get the CDF
#' @param seq_length The length of sequence to fit the distribution to
#' @param distributions The distributions to fit x against
#' @returns A dataframe with x, the real density, and the pdf of the desired
#'  distributions with length (nrows) equal to seq_length +1.
#' @examples
#' multicdf_cont(iris$Petal.Length)
#'
#' multicdf_cont(iris$Sepal.Length,
#'               100,
#'               c("normal", "lognormal")
#'               )
#' @export
multicdf_cont <- function(var, seq_length = 50, distributions = "all") {
  # init global vars
  var_seq <- NULL
  # get a sequence from the minimum to maximum of x with length
  #equal to seq_length + 1
  var_seq <- seq(min(var), max(var), length.out = seq_length + 1)
  # create real cumulative density for x
  var_cdf <- stats::ecdf(var)(var_seq)
  # initialize df of x and the cumulative density
  cdf_df <- as.data.frame(var_seq)
  cdf_df$dens <- var_cdf
  if ("all" %in% distributions) {
    distributions <- c("normal",
                       "lognormal",
                       "gamma",
                       "exponential")
  }
  if ("normal" %in% distributions) {
    var_n <- MASS::fitdistr(var, "normal")
    var_cdf_n <- stats::pnorm(var_seq, mean = var_n$estimate[1],
                              sd = var_n$estimate[2])
    cdf_df$cdf_normal <- var_cdf_n
  }
  if ("lognormal" %in% distributions) {
    var_ln <- MASS::fitdistr(var, "lognormal")
    var_cdf_ln <- stats::plnorm(var_seq, meanlog = var_ln$estimate[1],
                                sdlog = var_ln$estimate[2])
    cdf_df$cdf_lognormal <- var_cdf_ln
  }
  if ("gamma" %in% distributions) {
    var_g <- MASS::fitdistr(var, "gamma")
    var_cdf_g <- stats::pgamma(var_seq, shape = var_g$estimate[1],
                               rate = var_g$estimate[2])
    cdf_df$cdf_gamma <- var_cdf_g
  }
  if ("exponential" %in% distributions) {
    var_exp <- MASS::fitdistr(var, "exponential")
    var_cdf_exp <- stats::pexp(var_seq, rate = var_exp$estimate)
    cdf_df$cdf_exponential <- var_cdf_exp
  }
  return(cdf_df)
}
