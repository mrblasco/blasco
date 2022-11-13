#' Bottom-code variables
#'
#' Censor a variable below a certain lower bound
#'
#' @param x variable
#' @param value Lower bound
#' @export
bottomcode <- function(x,value) ifelse(x<value,value,x)

#' Top-code variables
#'
#' Censor a variable above a certain upper bound
#'
#' @param x Variable to be censored
#' @param value Upper bound
#' @export
topcode <- function(x,value) ifelse(x>value,value,x)

#' Inverse logistic function = exp(x) / (1 + exp(x))
#' @param x Real number
#' @export
#' @examples
#' curve(ilogit, from=-5, to=5)
ilogit <- function(x) exp(x) / (1 + exp(x))

#' Convert proportions in percentage strings with the `%` sign.
#'
#' Transforms proportions in strings with percent sign (with or without brackets). 
#'
#' @param x real number (proportion)
#' @param format to pass to the `sprintf` function
#' @export
#'
#' @examples
#' percent(0.15)
percent <- function(x, format="%0.1f") {
	sprintf(paste0(format,'%s'), 100 * x, "%")
}


#' Impute missing values
#'
#' Replace missing values with given value
#'
#' @param x Vector
#' @param value Replacement value
#' @export
#' @examples
#' impute(c(NA, 12, NA, 1:10), 0)
impute <- function(x, value) {
  if (all(!is.na(x))) return(x)
  x_imputed <- ifelse(is.na(x), value, x)
  attr(x_imputed, "value") <- value
  attr(x_imputed, "missing") <- which(is.na(x))
  return(x_imputed)
}


#' Pivot data from wide to long format
#' @export
pivot_longer <- function(data, varnames) {
	stopifnot(is.data.frame(data))
	out <- rep()
	for (v in varnames) out <- append(out, d[[v]])
	data.frame(row_id = rep(1:nrow(d), times=length(varnames))
		, varname = rep(varnames, each=nrow(d))
		, value = out)
}
