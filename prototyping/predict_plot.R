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
      scico::scale_color_scico_d(begin = 0.9, end = 0.1, palette = palette) +
      scico::scale_fill_scico_d(begin = 0.9, end = 0.1, palette = palette)
    
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
      scico::scale_color_scico(begin = 0.9, end = 0.1, palette = palette) +
      scico::scale_fill_scico(begin = 0.9, end = 0.1, palette = palette)
  }
  ### make the plot look good (group agnostic)
  p <- p +
    ggplot2::labs(
      title = paste("Observed data vs predicted 95%", interval, "interval"),
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

# testing
## grouped
mod1 <- lm(Sepal.Length ~ Sepal.Width + Species, data = iris)
predict_plot(mod1, iris, Sepal.Length, Sepal.Width, Species)
## ungrouped
mod2 <- lm(Sepal.Length ~ Sepal.Width, data = iris)
predict_plot(mod2, iris, Sepal.Length, Sepal.Width)
