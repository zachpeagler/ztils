#function testing
tfunc <- function(var) {
  if (grepl("[$]", deparse(substitute(var))) == TRUE) {
    var_name <- unlist(strsplit(deparse(substitute(var)), split="[$]"))[2]
  } else {
    var_name <- unlist(strsplit(deparse(substitute(var)), split="[\"]"))[2]
  }
  return(var_name)
}

t <- tfunc(iris$Sepal.Length)
t2 <- tfunc(iris[["Sepal.Length"]])


# predict plot testing
mod1 <- lm(Sepal.Length ~ Petal.Length + Species, data = iris)

predict_plot(mod1, iris, Sepal.Length, Petal.Length, Species)
