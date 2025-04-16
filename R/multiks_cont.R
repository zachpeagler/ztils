#' Multiple Kolmogorov-Smirnov Tests for Continuous Variables
#' @description
#' This function gets the distance and p-value from a Kolmogorov-smirnov
#' test for selected distributions against a continuous input variable.
#' Possible distributions include "normal",
#' "lognormal", "gamma", "exponential", and "all".
#'
#' @param var The variable to perform ks tests against
#' @param distributions The distributions to test x against
#' @returns A dataframe with the distance and p value for each performed
#' ks test
#' @examples
#' multiks_cont(iris$Sepal.Length)
#'
#' multiks_cont(iris$Sepal.Length, c("normal", "lognormal"))
#' @export
multiks_cont <- function(var, distributions = "all") {
  # check if "all" was passed to distributions
  if ("all" %in% distributions) {
    distributions <- c("normal",
                       "lognormal",
                       "gamma",
                       "exponential")
  }
  # dummy data frame
  ks_df <- data.frame(matrix(ncol = 3, nrow = 0))
  # set column names
  colnames(ks_df) <- c("Distribution", "Distance", "P-Value")
  # check normal
  if ("normal" %in% distributions) {
    var_n <- MASS::fitdistr(var, "normal")
    var_ks_n <- stats::ks.test(var, "pnorm", mean = var_n$estimate[1],
                               sd = var_n$estimate[2])
    ks_n <- data.frame(matrix(ncol = 0, nrow = 1))
    ks_n$Distribution <- "Normal"
    ks_n$Distance <- if (!is.null(var_ks_n$statistic)) {
      var_ks_n$statistic
    } else {
      "NA"
    }
    ks_n$PValue <- if (!is.null(var_ks_n$p.value)) {
      var_ks_n$p.value
    } else {
      "NA"
    }
    ks_df <- rbind(ks_df, ks_n)
  }
  # check lognormal
  if ("lognormal" %in% distributions) {
    var_ln <- MASS::fitdistr(var, "lognormal")
    var_ks_ln <- stats::ks.test(var, "plnorm",
                                meanlog = var_ln$estimate[1],
                                sdlog = var_ln$estimate[2])[c(1, 2)]
    ks_ln <- data.frame(matrix(ncol = 0, nrow = 1))
    ks_ln$Distribution <- "Lognormal"
    ks_ln$Distance <- if (!is.null(var_ks_ln$statistic)) {
      var_ks_ln$statistic
    } else {
      "NA"
    }
    ks_ln$PValue <- if (!is.null(var_ks_ln$p.value)) {
      var_ks_ln$p.value
    } else {
      "NA"
    }
    ks_df <- rbind(ks_df, ks_ln)
  }
  # check gamma
  if ("gamma" %in% distributions) {
    var_g <- MASS::fitdistr(var, "gamma")
    var_ks_g <- stats::ks.test(var, "pgamma",
                               shape = var_g$estimate[1],
                               rate = var_g$estimate[2])
    ks_g <- data.frame(matrix(ncol = 0, nrow = 1))
    ks_g$Distribution <- "Gamma"
    ks_g$Distance <- if (!is.null(var_ks_g$statistic)) {
      var_ks_g$statistic
    } else {
      "NA"
    }
    ks_g$PValue <- if (!is.null(var_ks_g$p.value)) {
      var_ks_g$p.value
    } else {
      "NA"
    }
    ks_df <- rbind(ks_df, ks_g)
  }
  # check exponential
  if ("exponential" %in% distributions) {
    var_exp <- MASS::fitdistr(var, "exponential")
    var_ks_exp <- stats::ks.test(var, "pexp", rate = var_exp$estimate)
    ks_exp <- data.frame(matrix(ncol = 0, nrow = 1))
    ks_exp$Distribution <- "Exponential"
    ks_exp$Distance <- if (!is.null(var_ks_exp$statistic)) {
      var_ks_exp$statistic
    } else {
      "NA"
    }
    ks_exp$PValue <- if (!is.null(var_ks_exp$p.value)) {
      var_ks_exp$p.value
    } else {
      "NA"
    }
    ks_df <- rbind(ks_df, ks_exp)
  }
  # make sure types are correct and rounded for interpretability
  ks_df$Distribution <- as.factor(ks_df$Distribution)
  ks_df$Distance <- as.numeric(ks_df$Distance)
  ks_df$PValue <- as.numeric(format(as.numeric(ks_df$PValue),
                                    scientific = FALSE))
  ks_df$Distance <- round(ks_df$Distance, 3)
  ks_df$PValue <- round(ks_df$PValue, 3)
  return(ks_df)
}
