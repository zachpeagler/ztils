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

#' multiPDF_Z
#' 
#' This function is a version of MultiPDF_plot from my MultiFitR package that is customized to use
#' formatting that I like. However, this formatting uses several packages that I didn't want to include in the 
#' base MultiFitR or MultiFitRgg packages.
#' 
#' @param x The variable to for which to plot PDFs
#' @param seq_length The number of points over which to fit x
#' @param distributions The distributions to fit x against
#' @param palette The color palette to use on the graph
#' @param var_name The variable name to use for x
#' @returns A plot showing the PDF of the selected variable against the selected distributions over the selected sequence length
#' @export
multiPDF_Z <- function (x, seq_length, distributions, palette, var_name) {
  # check if "all" was passed to distributions
  if ("all" %in% distributions) {
    distributions <- c("normal",
                       "lognormal",
                       "gamma",
                       "exponential")
  }
  # calculate PDFs
  data <- multiFitR::multiPDF_cont(x, seq_length, distributions)

  # create plot with real density
  p <- ggplot2::ggplot(data) +
    ggplot2::geom_line(aes(x=x_seq, y=dens, color="Real Density"), linetype = 2, linewidth = 3)+
    ggplot2::xlab(var_name)+
    ggplot2::ylab("PDF")+
    ggplot2::labs(title=paste("PDF plot for", var_name, "over selected distributions"))+
    ggplot2::guides(color=guide_legend(title="Distribution"))+
    ggplot2::theme_bw()
  # check for each type of distribution in the distributions, and add it if present
  if ("normal" %in% distributions == TRUE) {
    p <- p + ggplot2::geom_line(aes(x=x_seq, y=pdf_normal, color='Normal'), linewidth = 2)
  }
  if ("lognormal" %in% distributions == TRUE) {
    p <- p + ggplot2::geom_line(aes(x=x_seq, y=pdf_lognormal, color='Lognormal'), linewidth = 2)
  }
  if ("gamma" %in% distributions == TRUE) {
    p <- p + ggplot2::geom_line(aes(x=x_seq, y=pdf_gamma, color='Gamma'), linewidth = 2)
  }
  if ("exponential" %in% distributions == TRUE) {
    p <- p + ggplot2::geom_line(aes(x=x_seq, y=pdf_exponential, color='Exponential'), linewidth = 2)
  }
  p <- p +
    scico::scale_color_scico_d(begin=0.9, end=0, palette = palette)+
    ggplot2::theme(
      text = ggplot2::element_text(size=14, family="mont"),
      title = ggplot2::element_text(size=20, family = "mont", face = "bold"),
      legend.position="bottom",
      legend.title.position = "top",
      legend.title = ggplot2::element_text(size=16, family = "mont", face= "bold"),
      axis.title = ggplot2::element_text(size=16, family = "mont", face= "bold"),
    )
  return(p)
}

#' multiCDF_Z
#' 
#' This function is a version of multiCDF_plot from my MultiFitR package that is customized to use
#' formatting that I like. However, this formatting uses several packages that I didn't want to include in the 
#' base MultiFitR or MultiFitRgg packages. (showtext and scico)
#' 
#' @param x The variable to for which to plot CDFs
#' @param seq_length The number of points over which to fit x
#' @param distributions The distributions to fit x against
#' @param palette The color palette to use on the graph
#' @param var_name The variable name to use for x
#' @returns A plot showing the CDF of the selected variable against the selected distributions over the selected sequence length
#' @export
multiCDF_Z <- function (x, seq_length, distributions, palette, var_name) {
  # check if "all" was passed to distributions
  if ("all" %in% distributions) {
    distributions <- c("normal",
                       "lognormal",
                       "gamma",
                       "exponential")
  }
  # calculate CDFs
  data <- multiFitR::multiCDF_cont(x, seq_length, distributions)
  # create plot with real density
  p <- ggplot2::ggplot(data) +
    ggplot2::geom_line(aes(x=x_seq, y=dens, color="Real Distribution"), linetype = 2, linewidth = 3)+
    ggplot2::xlab(var_name)+
    ggplot2::ylab("CDF")+
    ggplot2::labs(title=paste("CDF plot for", var_name, "over selected distributions"))+
    ggplot2::guides(color=guide_legend(title="Distribution"))+
    ggplot2::theme_bw()
  # check for each type of distribution in the distributions, and add it if present
  if ("normal" %in% distributions == TRUE) {
    p <- p + ggplot2::geom_line(aes(x=x_seq, y=cdf_normal, color='Normal'), linewidth = 2)
  }
  if ("lognormal" %in% distributions == TRUE) {
    p <- p + ggplot2::geom_line(aes(x=x_seq, y=cdf_lognormal, color='Lognormal'), linewidth = 2)
  }
  if ("gamma" %in% distributions == TRUE) {
    p <- p + ggplot2::geom_line(aes(x=x_seq, y=cdf_gamma, color='Gamma'), linewidth = 2)
  }
  if ("exponential" %in% distributions == TRUE) {
    p <- p + ggplot2::geom_line(aes(x=x_seq, y=cdf_exponential, color='Exponential'), linewidth = 2)
  }
  p <- p +
    scico::scale_color_scico_d(begin=0.9, end=0, palette = palette)+
    ggplot2::theme(
      text = ggplot2::element_text(size=14, family="mont"),
      title = ggplot2::element_text(size=20, family = "mont", face = "bold"),
      legend.position="bottom",
      legend.title.position = "top",
      legend.title = ggplot2::element_text(size=16, family = "mont", face= "bold"),
      axis.title = ggplot2::element_text(size=16, family = "mont", face= "bold"),
    )
  return(p)
}

#predPlot <- function(model, seq_length, group, data) {
#  seq <- 
#  prx <- as.data.frame(x=seq)
#  pred <- predict(model, newdata=prx, se.fit=TRUE, type="link")
#}

#' pseudoR2
#' 
#' This function calculates pseudoR2s for GLMs