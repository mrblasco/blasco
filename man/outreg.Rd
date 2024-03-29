% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/outreg.R
\name{outreg}
\alias{outreg}
\title{Print regression results with notes}
\usage{
outreg(
  x,
  caption = NULL,
  label = NULL,
  notes = NULL,
  tab.layout = "-m#c-to-s-",
  ...
)
}
\arguments{
\item{x}{object or list of fitted regression objects (see stargazer manual)}

\item{caption}{string with table's title}

\item{label}{string with latex's label}

\item{...}{additional options (see stargazer)}
}
\description{
Wrapper based on \code{stargazer} to print regression tables.
}
\examples{
ols.1 <- lm(mpg ~ cyl, mtcars)
ols.2 <- update(ols.1, ~ . + gear)
ols.3 <- update(ols.1, ~ . + am + I(gear^2))
models <- list(baseline=ols.1, gear=ols.2, fe=ols.3)
outreg(models, column.labels=names(models)
       , caption="Table's caption", label="my label", notes="My notes")
}
