#' No outliers
#'
#' This function returns a dataframe subsetted to not include observations that
#' are beyond the outliers of the specified variable.
#'
#' Outliers are defined by the quantiles +- 1.5 times the interquartile range.
#'
#' @param data The data to subset
#' @param var The variable to subset by
#' @returns A dataframe without entries containing outliers in the selected variable.
#' @export
no_outliers <- function(data, var) {
  # deparse variable
  #d_var <- deparse(substitute(var))
  d_var <- deparse(substitute(var))
  # get quantiles
  Qvar <- quantile(data[[d_var]], probs=c(.25, .75), na.rm=TRUE)
  # get IQR
  iqr_var <- IQR(data[[d_var]])
  # subset df
  data <- subset(data, data[[d_var]] > (Qvar[1]-1.5*iqr_var) &
                 data[[d_var]] < (Qvar[2]+1.5*iqr_var))
  return(data)
}

#' No extremes
#'
#' This function returns a dataframe subsetted to not include observations that
#' are beyond the extremes of the specified variable.
#'
#' Extremes are defined by the quantiles +- 3 times the interquartile range.
#'
#' @param data The data to subset
#' @param var The variable to subset by.
#' @returns A dataframe without entries containing extremes in the selected variable.
#' @export
no_extremes <- function(data, var) {
  # deparse variable
  d_var <- deparse(substitute(var))
  # get quantiles
  Qvar <- quantile(data[[d_var]], probs=c(.25, .75), na.rm=TRUE)
  # get IQR
  iqr_var <- IQR(data[[d_var]])
  # subset data
  data <- subset(data, data[[d_var]] > (Qvar[1]-3*iqr_var) &
                 data[[d_var]] < (Qvar[2]+3*iqr_var))
  return(data)
}

#' Multiple Proportional Density Functions for Continuous Variables
#'
#' This function gets the proportional density functions for selected distributions
#' against continuous, non-negative numbers. Possible distributions include "normal",
#' "lognormal", "gamma", "exponential", and "all".
#'
#' @param var The variable of which to get the PDF.
#' @param seq_length The length of sequence to fit the distribution to
#' @param distributions The distributions to fit x against
#' @returns A dataframe with x, the real density, and the pdf of the desired
#'  distributions with length (nrows) equal to seq_length +1.
#' @export
multiPDF_cont <- function(var, seq_length = 50, distributions = "all"){
  # get a sequence from the minimum to maximum of x with length
  #equal to seq_length + 1
  var_seq <- seq(min(var), max(var), length.out = seq_length+1)
  # create real density for x
  var_pdf <- density(var, n=seq_length+1)
  # initialize df of x and the real density
  pdf_df <- as.data.frame(var_seq)
  pdf_df$dens = var_pdf$y
  ## see if "all" is in distributions
  if ("all" %in% distributions) {
    distributions <- c("normal", "lognormal", "gamma", "exponential")
  }

  if ("normal" %in% distributions) {
    var_n <- MASS::fitdistr(var, "normal")
    var_pdf_n <- dnorm(var_seq, mean=var_n$estimate[1],
                     sd = var_n$estimate[2])
    pdf_df$pdf_normal = var_pdf_n
  }
  if ("lognormal" %in% distributions) {
    var_ln <- MASS::fitdistr(var, "lognormal")
    var_pdf_ln <- dlnorm(var_seq, meanlog=var_ln$estimate[1],
                       sdlog = var_ln$estimate[2])
    pdf_df$pdf_lognormal = var_pdf_ln
  }
  if ("gamma" %in% distributions) {
    var_g <- MASS::fitdistr(var, "gamma")
    var_pdf_g <- dgamma(var_seq, shape=var_g$estimate[1],
                      rate=var_g$estimate[2])
    pdf_df$pdf_gamma = var_pdf_g
  }
  if ("exponential" %in% distributions) {
    var_exp <- MASS::fitdistr(var, "exponential")
    var_pdf_exp <- dexp(var_seq, rate = var_exp$estimate)
    pdf_df$pdf_exponential = var_pdf_exp
  }
  ## return dataframe with pdfs
  return(pdf_df)
}

#' Multiple Cumulative Distribution Functions for Continuous Variables
#'
#' This function gets the cumulative distribution function for selected distributions
#' against a continuous, non-negative input variable. Possible distributions include "normal",
#' "lognormal", "gamma", "exponential", "cauchy", "t", "weibull", "logistic",
#' and "all".
#'
#' @param var The variable of which to get the CDF
#' @param seq_length The length of sequence to fit the distribution to
#' @param distributions The distributions to fit x against
#' @returns A dataframe with x, the real density, and the pdf of the desired
#'  distributions with length (nrows) equal to seq_length +1.
#' @export
multiCDF_cont <- function(var, seq_length = 50, distributions = "all"){
  # get a sequence from the minimum to maximum of x with length
  #equal to seq_length + 1
  var_seq <- seq(min(var), max(var), length.out = seq_length+1)
  # create real cumulative density for x
  var_cdf <- ecdf(var)(var_seq)
  # initialize df of x and the cumulative density
  cdf_df <- as.data.frame(var_seq)
  cdf_df$dens = var_cdf
  if ("all" %in% distributions) {
    distributions <- c("normal",
                       "lognormal",
                       "gamma",
                       "exponential")
  }

  if ("normal" %in% distributions) {
    var_n <- MASS::fitdistr(var, "normal")
    var_cdf_n <- pnorm(var_seq, mean=var_n$estimate[1],
                     sd = var_n$estimate[2])
    cdf_df$cdf_normal = var_cdf_n
  }
  if ("lognormal" %in% distributions) {
    var_ln <- MASS::fitdistr(var, "lognormal")
    var_cdf_ln <- plnorm(var_seq, meanlog=var_ln$estimate[1],
                       sdlog = var_ln$estimate[2])
    cdf_df$cdf_lognormal = var_cdf_ln
  }
  if ("gamma" %in% distributions) {
    var_g <- MASS::fitdistr(var, "gamma")
    var_cdf_g <- pgamma(var_seq, shape=var_g$estimate[1],
                      rate=var_g$estimate[2])
    cdf_df$cdf_gamma = var_cdf_g
  }
  if ("exponential" %in% distributions) {
    var_exp <- MASS::fitdistr(var, "exponential")
    var_cdf_exp <- pexp(var_seq, rate = var_exp$estimate)
    cdf_df$cdf_exponential = var_cdf_exp
  }

  return(cdf_df)
}

#' Multiple Kolmogorov-Smirnov Tests for Continuous Variables
#'
#' This function gets the distance and p-value from a Kolmogorov-smirnov test for selected distributions
#' against a continuous, non-negative input variable. Possible distributions include "normal",
#' "lognormal", "gamma", "exponential", "logistic", and "all".
#'
#' @param var The variable to perform KS tests against
#' @param distributions The distributions to test x against
#' @returns A dataframe with the distance and p value for each performed
#' KS test
#' @export
multiKS_cont <- function(var, distributions = "all") {
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
    var_n <- MASS::fitdistr(var, "normal")
    var_KS_n <- ks.test(var, "pnorm", mean=var_n$estimate[1],
                      sd = var_n$estimate[2])
    KS_n <- data.frame(matrix(ncol=0, nrow=1))
    KS_n$Distribution <- "Normal"
    KS_n$Distance <- if (!is.null(var_KS_n$statistic)) {var_KS_n$statistic}
    else {"NA"}
    KS_n$PValue <- if (!is.null(var_KS_n$p.value)) {var_KS_n$p.value}
    else {"NA"}
    KS_df <- rbind(KS_df, KS_n)
  }
  if ("lognormal" %in% distributions) {
    var_ln <- MASS::fitdistr(var, "lognormal")
    var_KS_ln <- ks.test(var, "plnorm",
                       meanlog=var_ln$estimate[1],
                       sdlog = var_ln$estimate[2])[c(1, 2)]
    KS_ln <- data.frame(matrix(ncol=0, nrow=1))
    KS_ln$Distribution <- "Lognormal"
    KS_ln$Distance <- if (!is.null(var_KS_ln$statistic)) {var_KS_ln$statistic}
    else {"NA"}
    KS_ln$PValue <- if (!is.null(var_KS_ln$p.value)) {var_KS_ln$p.value}
    else {"NA"}
    KS_df <- rbind(KS_df, KS_ln)
  }
  if ("gamma" %in% distributions) {
    var_g <- MASS::fitdistr(var, "gamma")
    var_KS_g <- ks.test(var, "pgamma",
                        shape=var_g$estimate[1],
                        rate=var_g$estimate[2])
    KS_g <- data.frame(matrix(ncol=0, nrow=1))
    KS_g$Distribution <- "Gamma"
    KS_g$Distance <- if (!is.null(var_KS_g$statistic)) {var_KS_g$statistic}
                         else {"NA"}
    KS_g$PValue <- if (!is.null(var_KS_g$p.value)) {var_KS_g$p.value}
                      else {"NA"}
    KS_df <- rbind(KS_df, KS_g)
  }
  if ("exponential" %in% distributions) {
    var_exp <- MASS::fitdistr(var, "exponential")
    var_KS_exp <- ks.test(var, "pexp", rate = var_exp$estimate)
    KS_exp <- data.frame(matrix(ncol=0, nrow=1))
    KS_exp$Distribution <- "Exponential"
    KS_exp$Distance <- if (!is.null(var_KS_exp$statistic)) {var_KS_exp$statistic}
                        else {"NA"}
    KS_exp$PValue <- if (!is.null(var_KS_exp$p.value)) {var_KS_exp$p.value}
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

#'glm_pseudoR2
#'
#'A function for calculating the pseudo R^2 for a glm
#'
#'@param mod The model for which to calculate the pseudo R^2
#'@returns The pseudo R^2 value of the model
#'@export
glm_pseudoR2 <- function (mod) {
  1 - (mod$deviance/mod$null.deviance)
  }

#' multiPDF_plot
#'
#' This function is a version of MultiPDF_plot from my MultiFitR package that is customized to use
#' formatting that I like. However, this formatting uses several packages that I didn't want to include in the
#' base MultiFitR or MultiFitRgg packages.
#'
#' @param var The variable to for which to plot PDFs
#' @param seq_length The number of points over which to fit x
#' @param distributions The distributions to fit x against
#' @param palette The color palette to use on the graph
#' @param var_name The variable name to use for x. If no name is provided, the function will grab the column name provided in x
#' @returns A plot showing the PDF of the selected variable against the selected distributions over the selected sequence length
#' @export
multiPDF_plot <- function (var, seq_length = 50, distributions = "all", palette = "oslo", var_name = NULL) {
  # check if "all" was passed to distributions
  if ("all" %in% distributions) {
    distributions <- c("normal",
                       "lognormal",
                       "gamma",
                       "exponential")
  }
  # calculate PDFs
  data <- multiPDF_cont(var, seq_length, distributions)
  if (is.null(var_name)) {
    var_name <- unlist(strsplit(deparse(substitute(var)), split="[$]"))[2]
  }
  # create plot with real density
  p <- ggplot2::ggplot(data) +
    ggplot2::geom_line(aes(x=var_seq, y=dens, color="Real Density"), linetype = 2, linewidth = 3)+
    ggplot2::xlab(var_name)+
    ggplot2::ylab("PDF")+
    ggplot2::labs(title=paste("PDF plot for", var_name, "over selected distributions"))+
    ggplot2::guides(color=guide_legend(title="Distribution"))+
    ggplot2::theme_bw()
  # check for each type of distribution in the distributions, and add it if present
  if ("normal" %in% distributions == TRUE) {
    p <- p + ggplot2::geom_line(aes(x=var_seq, y=pdf_normal, color='Normal'), linewidth = 2)
  }
  if ("lognormal" %in% distributions == TRUE) {
    p <- p + ggplot2::geom_line(aes(x= var_seq, y=pdf_lognormal, color='Lognormal'), linewidth = 2)
  }
  if ("gamma" %in% distributions == TRUE) {
    p <- p + ggplot2::geom_line(aes(x= var_seq, y=pdf_gamma, color='Gamma'), linewidth = 2)
  }
  if ("exponential" %in% distributions == TRUE) {
    p <- p + ggplot2::geom_line(aes(x= var_seq, y=pdf_exponential, color='Exponential'), linewidth = 2)
  }
  p <- p +
    scico::scale_color_scico_d(begin=0.9, end=0, palette = palette)+
    ggplot2::theme(
      text = ggplot2::element_text(size=10, family="mont"),
      title = ggplot2::element_text(size=14, family = "mont", face = "bold"),
      legend.position="bottom",
      legend.title.position = "top",
      legend.title = ggplot2::element_text(size=12, family = "mont", face= "bold"),
      axis.title = ggplot2::element_text(size=12, family = "mont", face= "bold"),
    )
  return(p)
}

#' multiCDF_plot
#'
#' This function is a version of multiCDF_plot from my MultiFitR package that is customized to use
#' formatting that I like. However, this formatting uses several packages that I didn't want to include in the
#' base MultiFitR or MultiFitRgg packages. (showtext and scico)
#'
#' @param var The variable to for which to plot CDFs
#' @param seq_length The number of points over which to fit x
#' @param distributions The distributions to fit x against
#' @param palette The color palette to use on the graph
#' @param var_name The variable name to use for x
#' @returns A plot showing the CDF of the selected variable against the selected distributions over the selected sequence length
#' @export
multiCDF_plot <- function (var, seq_length = 50, distributions = "all", palette = "oslo", var_name = NULL) {
  # check if "all" was passed to distributions
  if ("all" %in% distributions) {
    distributions <- c("normal",
                       "lognormal",
                       "gamma",
                       "exponential")
  }
  # calculate CDFs
  data <- multiCDF_cont(var, seq_length, distributions)
  # if var_name is not provided, get it from the input variable
  if (is.null(var_name)) {
    var_name <- unlist(strsplit(deparse(substitute(var)), split="[$]"))[2]
  }
  # create plot with real density
  p <- ggplot2::ggplot(data) +
    ggplot2::geom_line(aes(x=var_seq, y=dens, color="Real Distribution"), linetype = 2, linewidth = 3)+
    ggplot2::xlab(var_name)+
    ggplot2::ylab("CDF")+
    ggplot2::labs(title=paste("CDF plot for", var_name, "over selected distributions"))+
    ggplot2::guides(color=guide_legend(title="Distribution"))+
    ggplot2::theme_bw()
  # check for each type of distribution in the distributions, and add it if present
  if ("normal" %in% distributions == TRUE) {
    p <- p + ggplot2::geom_line(aes(x=var_seq, y=cdf_normal, color='Normal'), linewidth = 2)
  }
  if ("lognormal" %in% distributions == TRUE) {
    p <- p + ggplot2::geom_line(aes(x=var_seq, y=cdf_lognormal, color='Lognormal'), linewidth = 2)
  }
  if ("gamma" %in% distributions == TRUE) {
    p <- p + ggplot2::geom_line(aes(x=var_seq, y=cdf_gamma, color='Gamma'), linewidth = 2)
  }
  if ("exponential" %in% distributions == TRUE) {
    p <- p + ggplot2::geom_line(aes(x=var_seq, y=cdf_exponential, color='Exponential'), linewidth = 2)
  }
  p <- p +
    scico::scale_color_scico_d(begin=0.9, end=0, palette = palette)+
    ggplot2::theme(
      text = ggplot2::element_text(size=10, family="mont"),
      title = ggplot2::element_text(size=14, family = "mont", face = "bold"),
      legend.position="bottom",
      legend.title.position = "top",
      legend.title = ggplot2::element_text(size=12, family = "mont", face= "bold"),
      axis.title = ggplot2::element_text(size=12, family = "mont", face= "bold"),
    )
  return(p)
}

#' Principal Component Analysis Plot
#'
#' This function uses a group, PCA variables, and a "scaled" boolean to generate
#' a biplot.
#' @param group The group variable (column)
#' @param pcavars The variables to include in the principle component analysis
#' @param scaled A boolean (TRUE or FALSE) indicating if the pcavars are already scaled
#' @returns A plot showing PC1 on the x axis, PC2 on the y axis, colored by group, with vectors and labels showing the individual pca variables.
#' @export
pca_plot <- function(group, pcavars, scaled = FALSE) {
  gr <- sort(unique(group))
  Groups <- gr[match(group, gr)]
  if (scaled == FALSE) {
    scaledvars <- data.frame(apply(pcavars, 2, scale))
    p1 <- rda(scaledvars)
  } else {
    p1 <- rda(pcavars)
  }
  PC1val <- round(p1$CA$eig[1]/sum(p1$CA$eig), 4)*100
  PC2val <- round(p1$CA$eig[2]/sum(p1$CA$eig), 4)*100
  px <- scores(p1)$sites
  vx <- scores(p1)$species
  ggplot()+
    geom_point(data=px, aes(x=PC1, y=PC2, color=Groups, fill=Groups), size=3)+
    geom_segment(data = vx, aes(x=0, y=0, xend=PC1*.18, yend=PC2*.18), color = "black")+
    annotate("text", x=vx[,1]*.2, y=vx[,2]*.2, label = rownames(vx))+
    xlim(-1, 1)+
    xlab(paste0("PC1 (", PC1val, "%)"))+
    ylab(paste0("PC2 (", PC2val, "%)"))+
    theme_bw()
}

#' Principal Component Analysis Data
#'
#' This function uses a dataframe, PCA variables, and a "scaled" boolean to generate
#' a dataframe with principal components as columns.
#' @param data The dataframe to add principal components to.
#' @param pcavars The variables to include in the principle component analysis
#' @param scaled A boolean (TRUE or FALSE) indicating if the pcavars are already scaled
#' @returns A plot showing PC1 on the x axis, PC2 on the y axis, colored by group, with vectors and labels showing the individual pca variables.
#' @export
pca_data <- function(data, pcavars, scaled = FALSE){
  if (scaled == FALSE) {
    scaledvars <- data.frame(apply(pcavars, 2, scale))
    p1 <- rda(scaledvars)
  } else {
    p1 <- rda(pcavars)
  }
  outdata <- cbind(data, p1$CA$u)
  return(outdata)
}

#' Prediction Plot
#'
#' This function uses a model, dataframe, and supplied predictor,response, and group variables to make predictions
#' based off the model over a user-defined length with options to predict over the confidence or prediction
#' interval and to apply a mathematical correction. You can also choose the color palette.
#'
#' This function natively uses ggplot2 to graph the plot.
#' This is because ggplot2 is beautiful and easy, and I hate making ugly (sorry) base R graphs.
#'
#' This function natively uses the scico package color palettes. I highly recommend scico for easy to use,
#' colorblind-accessible palettes.
#'
#' @param mod the model used for predictions
#' @param data the data used to render the "real" points on the graph and for aggregating groups to determine prediction limits (should be the same as the data used in the model)
#' @param rvar the response variable (y variable / variable the model is predicting)
#' @param pvar the predictor variable (x variable / variable the model will predict against)
#' @param group the group; should be a factor; one response curve will be made for each group
#' @param length the length of the variable over which to predict (higher = more resolution, essentially)
#' @param interval the type of interval to predict ("confidence" or "prediction")
#' @param correction the type of correction to apply to the prediction ("normal", "exponential", or "logit")
#' @returns A plot showing the real data and the model's predicted 95% CI or PI over a number of groups, with optional corrections.
#' @export
predict_plot <- function(mod, data, rvar, pvar, group = NULL, length = 50, interval = "confidence", correction = "normal") {
  if (!is.null(data[[deparse(substitute(group))]])){ ## grouped prediciton plot
    ### deparse variables
    d_pvar <- deparse(substitute(pvar))
    d_rvar <- deparse(substitute(rvar))
    d_group  <- deparse(substitute(group))
    ### get explicit names  of deparsed variables
    ### weird, but necessary for renaming the newdata (dx) columns \>_>/
    pvar_name <- colnames(data[d_pvar])
    rvar_name <- colnames(data[d_rvar])
    group_name  <- colnames(data[d_group])
    ## get group data ready
    groups  <- sort(unique(data[[d_group]]))
    ngroups <- length(groups)
    ## get predictor range for each group
    agg <- aggregate(data[[d_pvar]] ~ data[[d_group]], data = data, range)
    dx_pvar <- data.frame(pvar = numeric(0))
    for (i in 1:ngroups) {
      tpvar <- data.frame(pvar = seq(agg[[2]][i,1], agg[[2]][i,2], length = length))
      dx_pvar <- rbind(dx_pvar, tpvar)
    }
    dx <- data.frame(group = rep(agg[[1]], each = length),
                     pvar = dx_pvar)
    colnames(dx) <- c(group_name, pvar_name)
    ## make prediction
    if (interval == "confidence") {
      ### we don't need to explicitly declare that it's a confidence interval, the predict function defaults to it
      pred <- predict(mod, newdata = dx, se.fit = TRUE, type = "response")
      ### check for correction type
      if (correction == "exponential") {
        dx$mn <- exp(qnorm(0.5,   pred$fit, pred$se.fit))
        dx$lo <- exp(qnorm(0.025, pred$fit, pred$se.fit))
        dx$up <- exp(qnorm(0.975, pred$fit, pred$se.fit))
      } else if (correction == "logit") {
        dx$mn <- plogis(qnorm(0.5,   pred$fit, pred$se.fit))
        dx$lo <- plogis(qnorm(0.025, pred$fit, pred$se.fit))
        dx$up <- plogis(qnorm(0.975, pred$fit, pred$se.fit))
      } else {
        dx$mn <- qnorm(0.5,   pred$fit, pred$se.fit)
        dx$lo <- qnorm(0.025, pred$fit, pred$se.fit)
        dx$up <- qnorm(0.975, pred$fit, pred$se.fit)
      }
    } else { ### end confidence interval
      pred <- predict(mod, newdata = dx, se.fit = TRUE,
                      type = "response", interval = "prediction")
      ### check for correction type
      if (correction == "exponential") {
        dx$mn <- exp(pred$fit[,"fit"])
        dx$lo <- exp(pred$fit[,"lwr"])
        dx$up <- exp(pred$fit[,"upr"])
      } else if (correction == "logit") {
        dx$mn <- plogis(pred$fit[,"fit"])
        dx$lo <- plogis(pred$fit[,"lwr"])
        dx$up <- plogis(pred$fit[,"upr"])
      } else {
        dx$mn <- pred$fit[,"fit"]
        dx$lo <- pred$fit[,"lwr"]
        dx$up <- pred$fit[,"upr"]
      }
    } ### end prediction interval
    ## initialize plot with real data
    p <- ggplot2::ggplot() +
      ggplot2::geom_point(data = data, ggplot2::aes(x=.data[[d_pvar]], y=.data[[d_rvar]], color=.data[[d_group]]))
    ## loop through treatments
    for (g in 1:ngroups) {
      flag <- which(dx[[d_group]] == groups[g])
      tdx <- dx[flag,]
      p <- p +
        ggplot2::geom_line(data=tdx, ggplot2::aes(x=.data[[d_pvar]], y=lo, color = .data[[d_group]]),
                           linewidth=1, show.legend=FALSE)+
        ggplot2::geom_line(data=tdx, ggplot2::aes(x=.data[[d_pvar]], y=mn, color = .data[[d_group]]),
                           linewidth=2, show.legend=FALSE)+
        ggplot2::geom_line(data=tdx, ggplot2::aes(x=.data[[d_pvar]], y=up, color = .data[[d_group]]),
                           linewidth=1, show.legend=FALSE)+
        ggplot2::geom_ribbon(data=tdx, ggplot2::aes(x=.data[[d_pvar]], ymin=lo, ymax=up,
                                                    fill=.data[[d_group]]), alpha = 0.5)
    }
  } else { ### non-grouped prediction plot
    ### deparse variables
    d_pvar <- deparse(substitute(pvar))
    d_rvar <- deparse(substitute(rvar))
    ### get explicit names  of deparsed variables
    ### weird, but necessary for renaming the newdata (dx) columns \>_>/
    pvar_name <- colnames(data[d_pvar])
    rvar_name <- colnames(data[d_rvar])
    ## get predictor range
    dx_pvar <- seq(min(data[[d_pvar]]), max(data[[d_pvar]]), length)
    dx <- data.frame(pvar = dx_pvar)
    colnames(dx) <- pvar_name
    ## make prediction
    if (interval == "confidence") { ### confidence interval
      ### we don't need to explicitly declare that it's a confidence interval, the predict function defaults to it
      pred <- predict(mod, newdata = dx, se.fit = TRUE, type = "response")
      ### check for correction type
      if (correction == "exponential") {
        dx$mn <- exp(qnorm(0.5,   pred$fit, pred$se.fit))
        dx$lo <- exp(qnorm(0.025, pred$fit, pred$se.fit))
        dx$up <- exp(qnorm(0.975, pred$fit, pred$se.fit))
      } else if (correction == "logit") {
        dx$mn <- plogis(qnorm(0.5,   pred$fit, pred$se.fit))
        dx$lo <- plogis(qnorm(0.025, pred$fit, pred$se.fit))
        dx$up <- plogis(qnorm(0.975, pred$fit, pred$se.fit))
      } else {
        dx$mn <- qnorm(0.5,   pred$fit, pred$se.fit)
        dx$lo <- qnorm(0.025, pred$fit, pred$se.fit)
        dx$up <- qnorm(0.975, pred$fit, pred$se.fit)
      }
    } else { ### prediction interval
      pred <- predict(mod, newdata = dx, se.fit = TRUE,
                      type = "response", interval = "prediction")
      ### check for correction type
      if (correction == "exponential") {
        dx$mn <- exp(pred$fit[,"fit"])
        dx$lo <- exp(pred$fit[,"lwr"])
        dx$up <- exp(pred$fit[,"upr"])
      } else if (correction == "logit") {
        dx$mn <- plogis(pred$fit[,"fit"])
        dx$lo <- plogis(pred$fit[,"lwr"])
        dx$up <- plogis(pred$fit[,"upr"])
      } else {
        dx$mn <- pred$fit[,"fit"]
        dx$lo <- pred$fit[,"lwr"]
        dx$up <- pred$fit[,"upr"]
      }
    } ### end prediction interval
    ## initialize plot with real data
    p <- ggplot2::ggplot() +
      ggplot2::geom_point(data = data, ggplot2::aes(x=.data[[d_pvar]], y=.data[[d_rvar]], color=.data[[d_pvar]]))
    ## add prediction
    p <- p +
      ggplot2::geom_line(data=dx, ggplot2::aes(x=.data[[d_pvar]], y=lo),
                         linewidth=1, show.legend=FALSE)+
      ggplot2::geom_line(data=dx, ggplot2::aes(x=.data[[d_pvar]], y=mn),
                         linewidth=2, show.legend=FALSE)+
      ggplot2::geom_line(data=dx, ggplot2::aes(x=.data[[d_pvar]], y=up),
                         linewidth=1, show.legend=FALSE)+
      ggplot2::geom_ribbon(data=dx, ggplot2::aes(x=.data[[d_pvar]], ymin=lo, ymax=up), alpha = 0.5)
  } ### end non-grouped segment
  ### make the plot look good (group agnostic)
  p <- p +
    ggplot2::labs(
      title = paste("Observed data vs predicted 95%", interval, "interval"),
      subtitle = paste("Model:", deparse(mod$call))
    )+
    ggplot2::theme_bw()+
    ggplot2::theme(
      text = ggplot2::element_text(size=16),
      legend.position="right",
      axis.title = ggplot2::element_text(size=16, face= "bold"),
      title = ggplot2::element_text(size=20, face="bold", lineheight = .8),
      plot.subtitle = ggplot2::element_text(size=14, face = "italic")
    )
  return(p)
}
