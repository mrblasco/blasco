#' Bottom code a variable
#' @param x variable
#' @param value threshold
#' @export
bottomcode <- function(x,value) ifelse(x<value,value,x)

#' Top code a variable
#' @param x variable
#' @param value threshold
#' @export
topcode <- function(x,value) ifelse(x>value,value,x)

#' Inverse logistic function = exp(x) / (1 + exp(x))
#' @param x real number
#' @export
#' @examples
#' curve(ilogit, from=-5, to=5)
ilogit <- function(x) exp(x) / (1 + exp(x))


#' Add brackets
#'
#' Add () to any element of a vector
#'
#' @param x vector
#' @export
#'
#' @examples
#' brackets(format(runif(10),digits = 2))
brackets <- function(x) { 
  paste0("(", x,")")
}


#' Print proportions as percentages
#'
#' Transforms proportions in strings with percent sign (with or without brackets). 
#'
#' @param x real number (proportion)
#' @export
#'
#' @examples
#' p <- 0.15
#' percent(0.15)
percent <- function(x, digits=0, brackets=FALSE) { 
  pc <- paste0(round(x*100, digits), "%")
  if (brackets) pc <- paste0("(", pc,")")
  return(pc)
}






