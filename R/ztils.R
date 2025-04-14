#' No outliers
#' @description
#' This function returns a dataframe subsetted to not include observations that
#' are beyond the outliers of the specified variable.
#' Outliers are defined by the quantiles +- 1.5 times the interquartile range.
#'
#' @param data The data to subset
#' @param var The variable to subset by
#' @returns A dataframe without entries containing outliers in
#'  the selected variable.
#' @examples
#' no_outliers(iris, Sepal.Length)
#' @export
no_outliers <- function(data, var) {
  # deparse variable
  d_var <- deparse(substitute(var))
  # get quantiles
  q_var <- stats::quantile(data[[d_var]], probs = c(.25, .75), na.rm = TRUE)
  # get IQR
  iqr_var <- stats::IQR(data[[d_var]])
  # subset df
  data <- subset(data, data[[d_var]] > (q_var[1] - 1.5 * iqr_var) &
                   data[[d_var]] < (q_var[2] + 1.5 * iqr_var))
  return(data)
}

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

#' glm_pseudoR2
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

#' multipdf_plot
#' @description
#' This function extends 'multiPDF_cont' and gets the probability density
#' functions (PDFs) for selected distributions against continuous variables.
#' Possible distributions include any combination of "normal", "lognormal",
#' "gamma", "exponential", and "all" (which just uses all of the prior
#' distributions). It then plots this using 'ggplot2' and a 'scico' palette,
#' using var_name for the plot labeling, if specified. If not specified,
#' it will use var instead.
#'
#' @param var The variable to for which to plot PDFs
#' @param seq_length The number of points over which to fit x
#' @param distributions The distributions to fit x against
#' @param palette The color palette to use on the graph
#' @param var_name The variable name to use for x. If no name is provided,
#' the function will grab the column name provided in x
#' @returns A plot showing the PDF of the selected variable against the
#' selected distributions over the selected sequence length
#' @examples
#' multipdf_plot(iris$Sepal.Length)
#'
#' multipdf_plot(iris$Sepal.Length,
#'               seq_length = 100,
#'               distributions = c("normal", "lognormal", "gamma"),
#'               palette = "bilbao",
#'               var_name = "Sepal Length (cm)"
#'               )
#' @export
multipdf_plot <- function(var, seq_length = 50, distributions = "all",
                          palette = "oslo", var_name = NULL) {
  # initialize global variables
  var_seq <- dens <- NULL
  pdf_normal <- pdf_lognormal <- pdf_gamma <- pdf_exponential <- NULL
  # check if "all" was passed to distributions
  if ("all" %in% distributions) {
    distributions <- c("normal",
                       "lognormal",
                       "gamma",
                       "exponential")
  }
  # calculate PDFs
  data <- multipdf_cont(var, seq_length, distributions)
  if (is.null(var_name)) {
    if (grepl("[$]", deparse(substitute(var))) == TRUE) {
      var_name <- unlist(strsplit(deparse(substitute(var)), split = "[$]"))[2]
    } else {
      var_name <- unlist(strsplit(deparse(substitute(var)), split = "[\"]"))[2]
    }
  }
  # create plot with real density
  p <- ggplot2::ggplot(data) +
    ggplot2::geom_line(ggplot2::aes(x = var_seq, y = dens,
                                    color = "Real Density"),
                       linetype = 2, linewidth = 3) +
    ggplot2::xlab(var_name) +
    ggplot2::ylab("PDF") +
    ggplot2::labs(title = paste("PDF plot for", var_name,
                                "over selected distributions")) +
    ggplot2::guides(color = ggplot2::guide_legend(title = "Distribution")) +
    ggplot2::theme_bw()
  # check for each type of distribution in the distributions,
  # and add it if present
  if ("normal" %in% distributions == TRUE) {
    p <- p + ggplot2::geom_line(ggplot2::aes(x = var_seq, y = pdf_normal,
                                             color = "Normal"), linewidth = 2)
  }
  if ("lognormal" %in% distributions == TRUE) {
    p <- p + ggplot2::geom_line(ggplot2::aes(x = var_seq, y = pdf_lognormal,
                                             color = "Lognormal"),
                                linewidth = 2)
  }
  if ("gamma" %in% distributions == TRUE) {
    p <- p + ggplot2::geom_line(ggplot2::aes(x = var_seq, y = pdf_gamma,
                                             color = "Gamma"), linewidth = 2)
  }
  if ("exponential" %in% distributions == TRUE) {
    p <- p + ggplot2::geom_line(ggplot2::aes(x = var_seq, y = pdf_exponential,
                                             color = "Exponential"),
                                linewidth = 2)
  }
  p <- p +
    scico::scale_color_scico_d(begin = 0.8, end = 0.3, palette = palette) +
    ggplot2::theme(
      text = ggplot2::element_text(size = 10),
      title = ggplot2::element_text(size = 14, face = "bold"),
      legend.position = "bottom",
      legend.title.position = "top",
      legend.title = ggplot2::element_text(size = 12, face = "bold"),
      axis.title = ggplot2::element_text(size = 12, face = "bold"),
    )
  return(p)
}

#' multicdf_plot
#' @description
#' This function extends 'multiCDF_cont' and gets the cumulative distribution
#' functions (CDFs) for selected distributions against a continuous variable.
#' Possible distributions include any combination of "normal", "lognormal",
#' "gamma", "exponential", and "all" (which just uses all of the prior
#' distributions). It then plots this using 'ggplot2' and a 'scico' palette,
#' using var_name for the plot labeling, if specified. If not specified,
#' it will use var instead.
#'
#' @param var The variable to for which to plot CDFs
#' @param seq_length The number of points over which to fit x
#' @param distributions The distributions to fit x against
#' @param palette The color palette to use on the graph
#' @param var_name The variable name to use for x
#' @returns A plot showing the CDF of the selected variable against the
#' selected distributions over the selected sequence length
#' @examples
#' multicdf_plot(iris$Sepal.Length)
#'
#' multicdf_plot(iris$Sepal.Length,
#'               seq_length = 100,
#'               distributions = c("normal", "lognormal", "gamma"),
#'               palette = "bilbao",
#'               var_name = "Sepal Length (cm)"
#'               )
#' @export
multicdf_plot <- function(var, seq_length = 50, distributions = "all",
                          palette = "oslo", var_name = NULL) {
  # initialize global variables
  var_seq <- dens <- NULL
  cdf_normal <- cdf_lognormal <- cdf_gamma <- cdf_exponential <- NULL
  # check if "all" was passed to distributions
  if ("all" %in% distributions) {
    distributions <- c("normal",
                       "lognormal",
                       "gamma",
                       "exponential")
  }
  # calculate CDFs
  data <- multicdf_cont(var, seq_length, distributions)
  # if var_name is not provided, get it from the input variable
  if (is.null(var_name)) {
    if (grepl("[$]", deparse(substitute(var))) == TRUE) {
      var_name <- unlist(strsplit(deparse(substitute(var)), split = "[$]"))[2]
    } else {
      var_name <- unlist(strsplit(deparse(substitute(var)), split = "[\"]"))[2]
    }
  }
  # create plot with real density
  p <- ggplot2::ggplot(data) +
    ggplot2::geom_line(ggplot2::aes(x = var_seq, y = dens,
                                    color = "Real Distribution"),
                       linetype = 2, linewidth = 3) +
    ggplot2::xlab(var_name) +
    ggplot2::ylab("CDF") +
    ggplot2::labs(title = paste("CDF plot for", var_name,
                                "over selected distributions")) +
    ggplot2::guides(color = ggplot2::guide_legend(title = "Distribution")) +
    ggplot2::theme_bw()
  # check for each type of distribution in the distributions
  if ("normal" %in% distributions == TRUE) {
    p <- p + ggplot2::geom_line(ggplot2::aes(x = var_seq, y = cdf_normal,
                                             color = "Normal"), linewidth = 2)
  }
  if ("lognormal" %in% distributions == TRUE) {
    p <- p + ggplot2::geom_line(ggplot2::aes(x = var_seq, y = cdf_lognormal,
                                             color = "Lognormal"),
                                linewidth = 2)
  }
  if ("gamma" %in% distributions == TRUE) {
    p <- p + ggplot2::geom_line(ggplot2::aes(x = var_seq, y = cdf_gamma,
                                             color = "Gamma"), linewidth = 2)
  }
  if ("exponential" %in% distributions == TRUE) {
    p <- p + ggplot2::geom_line(ggplot2::aes(x = var_seq, y = cdf_exponential,
                                             color = "Exponential"),
                                linewidth = 2)
  }
  p <- p +
    scico::scale_color_scico_d(begin = 0.8, end = 0.3, palette = palette) +
    ggplot2::theme(
      text = ggplot2::element_text(size = 10),
      title = ggplot2::element_text(size = 14, face = "bold"),
      legend.position = "bottom",
      legend.title.position = "top",
      legend.title = ggplot2::element_text(size = 12, face = "bold"),
      axis.title = ggplot2::element_text(size = 12, face = "bold"),
    )
  return(p)
}

#' Principal Component Analysis Plot
#' @description
#' This function uses a group, PCA variables, and a scaled boolean to
#' generate a biplot.using 'ggplot2' and 'scico'.
#'
#' @param group The group variable (column)
#' @param pcavars The variables to include in the principle component analysis
#' @param scaled A boolean (TRUE or FALSE) indicating if the
#'  pcavars are already scaled
#' @param palette A color palette to use on the plot, with each
#'  group assigned to a color.
#' @returns A plot showing PC1 on the x axis, PC2 on the y axis, colored
#'  by group, with vectors and labels showing the individual pca variables.
#' @examples
#' pca_plot(iris$Species, iris[,c(1:4)])
#'
#' pca_plot(iris$Species, iris[,c(1:4)], FALSE, "bilbao")
#' @export
pca_plot <- function(group, pcavars, scaled = FALSE, palette = "oslo") {
  # init global variables
  PC1 <- PC2 <- NULL
  # prepare groups
  gr <- sort(unique(group))
  groups <- gr[match(group, gr)]
  # prepare scaled logical
  scaled <- as.logical(scaled)
  # check for scaled and perform pca
  if (scaled == FALSE) {
    scaledvars <- data.frame(apply(pcavars, 2, scale))
    p1 <- vegan::rda(scaledvars)
  } else {
    p1 <- vegan::rda(pcavars)
  }
  # get PC1 and PC2 values
  pc1val <- round(p1$CA$eig[1] / sum(p1$CA$eig), 4) * 100
  pc2val <- round(p1$CA$eig[2] / sum(p1$CA$eig), 4) * 100
  # get sites and species
  px <- vegan::scores(p1)$sites
  vx <- vegan::scores(p1)$species
  # plot
  p <- ggplot2::ggplot() +
    ggplot2::geom_point(data = px, ggplot2::aes(x = PC1, y = PC2,
                                                color = groups),
                        size = 3) +
    ggplot2::geom_segment(data = vx,
                          ggplot2::aes(x = 0, y = 0,
                                       xend = PC1 * .25, yend = PC2 * .25),
                          color = "black") +
    ggplot2::annotate("text", x = vx[, 1] * .27, y = vx[, 2] * .27,
                      label = rownames(vx)) +
    ggplot2::xlab(paste0("PC1 (", pc1val, "%)")) +
    ggplot2::ylab(paste0("PC2 (", pc2val, "%)")) +
    scico::scale_color_scico_d(begin = 0.8, end = 0.3, palette = palette) +
    ggplot2::guides(color = ggplot2::guide_legend(title = "Groups")) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      text = ggplot2::element_text(size = 10),
      legend.title = ggplot2::element_text(size = 12, face = "bold"),
      axis.title = ggplot2::element_text(size = 12, face = "bold"),
    )
  return(p)
}

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

#' Prediction Plot
#' @description
#' This function uses a model, dataframe, and supplied predictor, response,
#' and group variables to make predictions based off the model over a
#' user-defined length with options to predict over the confidence or
#' prediction interval and to apply a mathematical correction. It then
#' graphs both the real data and the specified interval using 'ggplot2'.
#' You can also choose the color palette from 'scico' palettes.
#'
#' @param mod the model used for predictions
#' @param data the data used to render the "real" points on the graph and
#' for aggregating groups to determine prediction limits
#' (should be the same as the data used in the model)
#' @param rvar the response variable (y variable / variable
#' the model is predicting)
#' @param pvar the predictor variable (x variable / variable the
#' model will predict against)
#' @param group the group; should be a factor; one response curve
#'  will be made for each group
#' @param length the length of the variable over which to predict
#' (higher = more resolution, essentially)
#' @param interval the type of interval to predict
#' ("confidence" or "prediction")
#' @param correction the type of correction to apply to the prediction
#' ("normal", "exponential", or "logit")
#' @param palette the color palette used to color the graph,
#' with each group corresponding to a color
#' @returns A plot showing the real data and the model's predicted 95% CI or PI
#' over a number of groups, with optional corrections.
#' @examples
#' ## Example 1
#' mod1 <- lm(Sepal.Length ~ Petal.Length + Species, data = iris)
#' predict_plot(mod1, iris, Sepal.Length, Petal.Length, Species)
#' @export
predict_plot <- function(mod, data, rvar, pvar, group = NULL,
                         length = 50, interval = "confidence",
                         correction = "normal", palette = "oslo") {
  # init global vars
  .data <- grouped <- lo <- mn <- up <- NULL
  ## deparse variables
  d_pvar <- deparse(substitute(pvar))
  d_rvar <- deparse(substitute(rvar))
  ## get explicit names  of deparsed variables
  pvar_name <- colnames(data[d_pvar])
  if (!is.null(data[[deparse(substitute(group))]])) { # group setup
    grouped <- TRUE
    d_group  <- deparse(substitute(group))
    group_name  <- colnames(data[d_group])
    ## get group data ready
    groups  <- sort(unique(data[[d_group]]))
    ngroups <- length(groups)
    ## get predictor range for each group
    agg <- stats::aggregate(data[[d_pvar]] ~ data[[d_group]],
                            data = data, range)
    dx_pvar <- data.frame(pvar = numeric(0))
    for (i in 1:ngroups) {
      tpvar <- data.frame(pvar = seq(agg[[2]][i, 1], agg[[2]][i, 2],
                                     length = length))
      dx_pvar <- rbind(dx_pvar, tpvar)
    }
    dx <- data.frame(group = rep(agg[[1]], each = length),
                     pvar = dx_pvar)
    colnames(dx) <- c(group_name, pvar_name)
  } else { # non group setup
    grouped <- FALSE
    dx_pvar <- seq(min(data[[d_pvar]]), max(data[[d_pvar]]),
                   length.out = length)
    dx <- data.frame(pvar = dx_pvar)
    colnames(dx) <- pvar_name
  }
  ## make prediction
  if (interval == "confidence") {
    pred <- stats::predict(mod, newdata = dx,
                           se.fit = TRUE, type = "response")
    ### check for correction type
    if (correction == "exponential") {
      dx$mn <- exp(stats::qnorm(0.5,   pred$fit, pred$se.fit))
      dx$lo <- exp(stats::qnorm(0.025, pred$fit, pred$se.fit))
      dx$up <- exp(stats::qnorm(0.975, pred$fit, pred$se.fit))
    } else if (correction == "logit") {
      dx$mn <- stats::plogis(stats::qnorm(0.5,   pred$fit, pred$se.fit))
      dx$lo <- stats::plogis(stats::qnorm(0.025, pred$fit, pred$se.fit))
      dx$up <- stats::plogis(stats::qnorm(0.975, pred$fit, pred$se.fit))
    } else {
      dx$mn <- stats::qnorm(0.5,   pred$fit, pred$se.fit)
      dx$lo <- stats::qnorm(0.025, pred$fit, pred$se.fit)
      dx$up <- stats::qnorm(0.975, pred$fit, pred$se.fit)
    }
  } else { ### end confidence interval
    pred <- stats::predict(mod, newdata = dx, se.fit = TRUE,
                           type = "response", interval = "prediction")
    ### check for correction type
    if (correction == "exponential") {
      dx$mn <- exp(pred$fit[, "fit"])
      dx$lo <- exp(pred$fit[, "lwr"])
      dx$up <- exp(pred$fit[, "upr"])
    } else if (correction == "logit") {
      dx$mn <- stats::plogis(pred$fit[, "fit"])
      dx$lo <- stats::plogis(pred$fit[, "lwr"])
      dx$up <- stats::plogis(pred$fit[, "upr"])
    } else {
      dx$mn <- pred$fit[, "fit"]
      dx$lo <- pred$fit[, "lwr"]
      dx$up <- pred$fit[, "upr"]
    }
  } ### end prediction interval
  ## initialize plot
  if (grouped == TRUE) {
    p <- ggplot2::ggplot() +
      ggplot2::geom_point(data = data, ggplot2::aes(x = .data[[d_pvar]],
                                                    y = .data[[d_rvar]],
                                                    color = .data[[d_group]]))
    ## loop through treatments
    for (g in 1:ngroups) {
      flag <- which(dx[[d_group]] == groups[g])
      tdx <- dx[flag, ]
      p <- p +
        ggplot2::geom_line(data = tdx, ggplot2::aes(x = .data[[d_pvar]], y = lo,
                                                    color = .data[[d_group]]),
                           linewidth = 1, show.legend = FALSE) +
        ggplot2::geom_line(data = tdx, ggplot2::aes(x = .data[[d_pvar]], y = mn,
                                                    color = .data[[d_group]]),
                           linewidth = 2, show.legend = FALSE) +
        ggplot2::geom_line(data = tdx, ggplot2::aes(x = .data[[d_pvar]], y = up,
                                                    color = .data[[d_group]]),
                           linewidth = 1, show.legend = FALSE) +
        ggplot2::geom_ribbon(data = tdx, ggplot2::aes(x = .data[[d_pvar]],
                                                      ymin = lo, ymax = up,
                                                      fill = .data[[d_group]]),
                             alpha = 0.5)
    }
    p <- p +
      scico::scale_color_scico_d(begin = 0.8, end = 0.3, palette = palette) +
      scico::scale_fill_scico_d(begin = 0.8, end = 0.3, palette = palette)

  } else {
    p <- ggplot2::ggplot() +
      ggplot2::geom_point(data = data, ggplot2::aes(x = .data[[d_pvar]],
                                                    y = .data[[d_rvar]],
                                                    color = .data[[d_pvar]]))
    ## add prediction
    p <- p +
      ggplot2::geom_line(data = dx, ggplot2::aes(x = .data[[d_pvar]], y = lo),
                         linewidth = 1, show.legend = FALSE) +
      ggplot2::geom_line(data = dx, ggplot2::aes(x = .data[[d_pvar]], y = mn),
                         linewidth = 2, show.legend = FALSE) +
      ggplot2::geom_line(data = dx, ggplot2::aes(x = .data[[d_pvar]], y = up),
                         linewidth = 1, show.legend = FALSE) +
      ggplot2::geom_ribbon(data = dx, ggplot2::aes(x = .data[[d_pvar]],
                                                   ymin = lo, ymax = up),
                           alpha = 0.5) +
      scico::scale_color_scico(begin = 0.8, end = 0.3, palette = palette) +
      scico::scale_fill_scico(begin = 0.8, end = 0.3, palette = palette)
  }
  ### make the plot look good (group agnostic)
  p <- p +
    ggplot2::labs(title = paste("Observed data vs predicted 95%",
                                interval, "interval"),
                  subtitle = paste("Model:", deparse(mod$call))) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      text = ggplot2::element_text(size = 12),
      axis.title = ggplot2::element_text(size = 14, face = "bold"),
      title = ggplot2::element_text(size = 16, face = "bold"),
      plot.subtitle = ggplot2::element_text(size = 14, face = "italic")
    )
  return(p)
}
