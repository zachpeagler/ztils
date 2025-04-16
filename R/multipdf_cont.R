#' Multiple Proportional Density Functions for Continuous Variables
#' @description
#' This function gets the proportional density functions for selected
#' distributions against continuous, non-negative numbers.
#' Possible distributions include "normal",
#' "lognormal", "gamma", "exponential", and "all".
#'
#' @param var The variable of which to get the PDF.
#' @param seq_length The length of sequence to fit the distribution to
#' @param distributions The distributions to fit x against
#' @returns A dataframe with x, the real density, and the pdf of the desired
#'  distributions with length (nrows) equal to seq_length +1.
#' @examples
#' multipdf_cont(iris$Petal.Length)
#'
#' multipdf_cont(iris$Sepal.Length, 100, c("normal", "lognormal"))
#' @export
multipdf_cont <- function(var, seq_length = 50, distributions = "all") {
  # get a sequence from the minimum to maximum of x with length
  #equal to seq_length + 1
  var_seq <- seq(min(var), max(var), length.out = seq_length + 1)
  # create real density for x
  var_pdf <- stats::density(var, n = seq_length + 1)
  # initialize df of x and the real density
  pdf_df <- as.data.frame(var_seq)
  pdf_df$dens <- var_pdf$y
  ## see if "all" is in distributions
  if ("all" %in% distributions) {
    distributions <- c("normal", "lognormal", "gamma", "exponential")
  }
  if ("normal" %in% distributions) {
    var_n <- MASS::fitdistr(var, "normal")
    var_pdf_n <- stats::dnorm(var_seq, mean = var_n$estimate[1],
                              sd = var_n$estimate[2])
    pdf_df$pdf_normal <- var_pdf_n
  }
  if ("lognormal" %in% distributions) {
    var_ln <- MASS::fitdistr(var, "lognormal")
    var_pdf_ln <- stats::dlnorm(var_seq, meanlog = var_ln$estimate[1],
                                sdlog = var_ln$estimate[2])
    pdf_df$pdf_lognormal <- var_pdf_ln
  }
  if ("gamma" %in% distributions) {
    var_g <- MASS::fitdistr(var, "gamma")
    var_pdf_g <- stats::dgamma(var_seq, shape = var_g$estimate[1],
                               rate = var_g$estimate[2])
    pdf_df$pdf_gamma <- var_pdf_g
  }
  if ("exponential" %in% distributions) {
    var_exp <- MASS::fitdistr(var, "exponential")
    var_pdf_exp <- stats::dexp(var_seq, rate = var_exp$estimate)
    pdf_df$pdf_exponential <- var_pdf_exp
  }
  ## return dataframe with pdfs
  return(pdf_df)
}
