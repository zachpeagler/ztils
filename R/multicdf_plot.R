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
    ggplot2::labs(title = paste("CDF for", var_name,
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
    scico::scale_color_scico_d(begin = 0.9, end = 0.2, palette = palette) +
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
