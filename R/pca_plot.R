#' Principal Component Analysis Plot
#' @description
#' This function uses a group, PCA variables, and a 'scaled' boolean to
#' generate a biplot using 'ggplot2' and 'scico'.
#'
#' If scaled is set to TRUE, variables will not be scaled. If scaled is set to
#' FALSE, variables will be scaled.
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
    scico::scale_color_scico_d(begin = 0.9, end = 0.2, palette = palette) +
    ggplot2::guides(color = ggplot2::guide_legend(title = "Groups")) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      text = ggplot2::element_text(size = 10),
      legend.title = ggplot2::element_text(size = 12, face = "bold"),
      axis.title = ggplot2::element_text(size = 12, face = "bold"),
    )
  return(p)
}
