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

#' Multiple Kolmogorov-Smirnov Tests for Continuous Variables
#'
#' This function gets the distance and p-value from a Kolmogorov-smirnov test for selected distributions
#' against a continuous, non-negative input variable. Possible distributions include "normal",
#' "lognormal", "gamma", "exponential", "cauchy", "t", "weibull", "logistic",
#' and "all".
#'
#' @param x The variable to perform KS tests against
#' @param distributions The distributions to test x against
#' @returns A dataframe with the distance and p value for each performed
#' KS test
#' @export
multiKS_cont <- function(x, distributions) {
  # check if "all" was passed to distributions
  if ("all" %in% distributions) {
    distributions <- c("normal",
                       "lognormal",
                       "gamma",
                       "exponential")
  }
  KS_df <- data.frame(matrix(ncol=3, nrow=0))
  colnames(KS_df) <- c("Distribution", "Distance", "P-Value")
  # check normal
  if ("normal" %in% distributions) {
    x_n <- MASS::fitdistr(x, "normal")
    x_KS_n <- ks.test(x, "pnorm", mean=x_n$estimate[1],
                      sd = x_n$estimate[2])
    KS_n <- data.frame(matrix(ncol=0, nrow=1))
    KS_n$Distribution <- "Normal"
    KS_n$Distance <- if (is.null(x_KS_n$statistic)
                         == FALSE) {x_KS_n$statistic}
    else {"NA"}
    KS_n$PValue <- if (is.null(x_KS_n$p.value)
                       == FALSE) {x_KS_n$p.value}
    else {"NA"}
    KS_df <- rbind(KS_df, KS_n)
  }
  if ("lognormal" %in% distributions) {
    x_ln <- MASS::fitdistr(x, "lognormal")
    x_KS_ln <- ks.test(x, "plnorm",
                       meanlog=x_ln$estimate[1],
                       sdlog = x_ln$estimate[2])[c(1, 2)]
    KS_ln <- data.frame(matrix(ncol=0, nrow=1))
    KS_ln$Distribution <- "Lognormal"
    KS_ln$Distance <- if (is.null(x_KS_ln$statistic)
                          == FALSE) {x_KS_ln$statistic}
    else {"NA"}
    KS_ln$PValue <- if (is.null(x_KS_ln$p.value)
                        == FALSE) {x_KS_ln$p.value}
    else {"NA"}
    KS_df <- rbind(KS_df, KS_ln)
  }
  if ("gamma" %in% distributions) {
    x_g <- MASS::fitdistr(x, "gamma")
    x_KS_g <- ks.test(x, "pgamma", shape=x_g$estimate[1],
                      rate=x_g$estimate[2])
    KS_g <- data.frame(matrix(ncol=0, nrow=1))
    KS_g$Distribution <- "Gamma"
    KS_g$Distance <- if (is.null(x_KS_g$statistic)
                         == FALSE) {x_KS_g$statistic}
    else {"NA"}
    KS_g$PValue <- if (is.null(x_KS_g$p.value)
                       == FALSE) {x_KS_g$p.value}
    else {"NA"}
    KS_df <- rbind(KS_df, KS_g)
  }
  if ("exponential" %in% distributions) {
    x_exp <- MASS::fitdistr(x, "exponential")
    x_KS_exp <- ks.test(x, "pexp", rate = x_exp$estimate)
    KS_exp <- data.frame(matrix(ncol=0, nrow=1))
    KS_exp$Distribution <- "Exponential"
    KS_exp$Distance <- if (is.null(x_KS_exp$statistic)
                           == FALSE) {x_KS_exp$statistic}
    else {"NA"}
    KS_exp$PValue <- if (is.null(x_KS_exp$p.value)
                         == FALSE) {x_KS_exp$p.value}
    else {"NA"}
    KS_df <- rbind(KS_df, KS_exp)
  }
  
  KS_df$Distribution = as.factor(KS_df$Distribution)
  KS_df$Distance = as.numeric(KS_df$Distance)
  KS_df$PValue = as.numeric(format(as.numeric(KS_df$PValue),
                                   scientific = FALSE))
  KS_df$Distance <- round(KS_df$Distance, 3)
  KS_df$PValue <- round(KS_df$PValue, 3)
  
  return(KS_df)
}

#predPlot <- function(model, seq_length, group, data) {
#  seq <- 
#  prx <- as.data.frame(x=seq)
#  pred <- predict(model, newdata=prx, se.fit=TRUE, type="link")
#}

#' pseudoR2
#' 
#' This function calculates pseudoR2s for GLMs