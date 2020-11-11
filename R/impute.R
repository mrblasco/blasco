#' Impute missing values
#'
#' Replace missing values with given value
#' @param x vector
#' @param value replacement value (="median" will impute the median of the non-missing values)
#' @export
#'
#' @examples
#' impute(c(NA, 12, NA, 1:10), 0)
impute <- function(x, value) { 
  if (value == "median") value <- median(x, na.rm = TRUE)
  x_imputed <- ifelse(is.na(x), value, x)
  attr(x_imputed, "value") <- value
  return(x_imputed)
}
