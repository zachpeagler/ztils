#' No extremes
#' 
#' This function returns a dataframe subsetted to not include observations that
#' are beyond the extremes of the specified variable.
#' 
#' Extremes are defined by the quantiles +- 3 times the interquartile range.
#' 
#' @param df The dataframe to subset
#' @param x The variable to subset by
#' @export
no_extremes <- function(df, x) {
  # get quantiles
  Qx <- quantile(x, probs=c(.25, .75), na.rm=FALSE)
  # get IQR
  iqr_x <- IQR(x)
  # subset df
  df <- subset(df, x > (Qx[1]-3.0*iqr_x) &
                 x < (Qx[2]+3.0*iqr_x))
  return(df)
}

#' No outliers
#' 
#' This function returns a dataframe subsetted to not include observations that
#' are beyond the outliers of the specified variable.
#' 
#' Outliers are defined by the quantiles +- 1.5 times the interquartile range.
#' 
#' @param df The dataframe to subset
#' @param x The variable to subset by
#' @export
no_outliers <- function(df, x) {
  # get quantiles
  Qx <- quantile(x, probs=c(.25, .75), na.rm=FALSE)
  # get IQR
  iqr_x <- IQR(x)
  # subset df
  df <- subset(df, x > (Qx[1]-1.5*iqr_x) &
                 x < (Qx[2]+1.5*iqr_x))
  return(df)
}
