#' Principal Component Analysis Data
#' @description
#' This function uses a dataframe, PCA variables, and a scaled boolean to
#'  generate a dataframe with principal components as columns.
#'
#' @param data The dataframe to add principal components to.
#' @param pcavars The variables to include in the principle component analysis
#' @param scaled A boolean (TRUE or FALSE) indicating if the
#'  pcavars are already scaled
#' @returns A plot showing PC1 on the x axis, PC2 on the y axis,
#'  colored by group, with vectors and labels showing the
#'  individual pca variables.
#' @examples
#' pca_data(iris, iris[,c(1:4)], FALSE)
#' @export
pca_data <- function(data, pcavars, scaled = FALSE) {
  scaled <- as.logical(scaled)
  if (scaled == FALSE) {
    scaledvars <- data.frame(apply(pcavars, 2, scale))
    p1 <- vegan::rda(scaledvars)
  } else {
    p1 <- vegan::rda(pcavars)
  }
  outdata <- cbind(data, p1$CA$u)
  return(outdata)
}
