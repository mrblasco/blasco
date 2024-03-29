% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{impute}
\alias{impute}
\title{Impute missing values}
\usage{
impute(x, value)
}
\arguments{
\item{x}{Vector}

\item{value}{Replacement value}
}
\description{
Replace missing values with given value
}
\examples{
impute(c(NA, 12, NA, 1:10), 0)
}
