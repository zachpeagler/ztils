#' Pseudo R^2 for Generalized Linear Model
#' @description
#' A function for calculating the pseudo R^2 of a glm object
#'
#' @param mod The model for which to calculate the pseudo R^2
#' @returns The pseudo R^2 value of the model
#' @examples
#' gmod <- glm(Sepal.Length ~ Petal.Length + Species, data = iris)
#' glm_pseudor2(gmod)
#' @export
glm_pseudor2 <- function(mod) {
  pr2 <- 1 - (mod$deviance / mod$null.deviance)
  return(pr2)
}
