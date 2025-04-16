#' No extremes
#' @description
#' This function returns a dataframe subsetted to not include observations that
#' are beyond the extremes of the specified variable.
#' Extremes are defined by the quantiles +- 3 times the interquartile range.
#'
#' @param data The data to subset
#' @param var The variable to subset by.
#' @returns A dataframe without entries containing extremes in
#'  the selected variable.
#' @examples
#' no_extremes(iris, Sepal.Length)
#'
#' @export
no_extremes <- function(data, var) {
  # deparse variable
  d_var <- deparse(substitute(var))
  # get quantiles
  q_var <- stats::quantile(data[[d_var]], probs = c(.25, .75), na.rm = TRUE)
  # get IQR
  iqr_var <- stats::IQR(data[[d_var]])
  # subset data
  data <- subset(data, data[[d_var]] > (q_var[1] - 3 * iqr_var) &
                   data[[d_var]] < (q_var[2] + 3 * iqr_var))
  return(data)
}
