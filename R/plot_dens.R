#  plot_dens
#'
#' Plot kernel density curve with black and white style with shaded area
#'
#' @param x it can be the data from which the estimate is to be computed, or an 'density' object
#'
#' @param rescale if TRUE, ensures density stays in [0,1]; default true.
#' @param col.area color of the shaded area; default "dodgerblue".
#'
#' @param ... Optional graphics arguments
#'
#' @export
#'
#' @return
#' None.
plot_dens <- function(x, rescale = TRUE, col.area = "dodgerblue", ylab = "density", ...) { 
  x_dens <- switch(class(x), "density" = x, density(x))
  if (rescale) { 
    x_dens$y <- x_dens$y/max(x_dens$y)
  }
  blasco::bwplot(x_dens$x, x_dens$y, type = "n", ylab = ylab, ...)
  x_vec <- c(min(x_dens$x), x_dens$x, max(x_dens$x))
  y_vec <- c(min(x_dens$y), x_dens$y, min(x_dens$y))
  polygon(x_vec, y_vec, col = col.area, border = NA)
  invisible()
}
