#' Pipe-friendly version of gsub function
#'
#' Performs replacement of patterns.
#'
#' @param x strings
#' @param pattern regex strings
#' @param replace replacement
#' @param ... other parameters to gsub
#' @export
str_replace <- function(x, pattern, replace, ...) {
	gsub(x=x, pattern=pattern, replace=replace, ...)
}


#' Wrap strings
#' @export
str_wrap <- function(x, width=18) {
	if (length(x) > 1) sapply(x, str_wrap)
	else paste(strwrap(x, width=width), collapse = "\n")
}
