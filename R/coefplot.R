#' Coefficient plot
#'
#' Plots estimates and coefficients from fitted object using `coef(summary())`
#'
#' @param x object with fitted model
#' @param omit string with regular expression to omit coefficients
#' @param horizontal estimates are plotted on the x-axis (default) or the y-axis
#' @export
coefplot <- function(object, omit=NULL, horizontal=TRUE, labels=TRUE, ...) {
  cf <- as.data.frame(coef(summary(object)))
  if(!is.null(omit)) cf <- cf[!grepl(omit, rownames(cf)), ]
  est <- cf[, 1]
  hi <- cf[, 1] + cf[, 2]
  lo <- cf[, 1] - cf[, 2]
  n <- length(est)
  est.labels <- NULL
  if (labels)
    est.labels <- substr(rownames(cf), nchar(rownames(cf))-10, nchar(rownames(cf)))
  if (horizontal) {
    plot(y=1:length(est), x=est, yaxt="n", ylab="", ...)
    segments(y0=1:length(est), x0=hi, x1=lo)
    text(y=1:length(est), x=est, labels=est.labels, adj=c(.5, 2), xpd=T)
    abline(v=0, lty=2)
  } else {
    plot(x=1:length(est), y=est, ...)
    segments(x0=1:length(est), y0=hi, y1=lo)
    text(x=1:length(est), est, est.labels, srt=90, adj=c(.5,1))
    abline(h=0, lty=2)
  }
}
