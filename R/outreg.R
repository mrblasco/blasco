#' Print regression results with notes
#'
#' Wrapper based on `stargazer` to print regression tables. 
#'
#' @param x object or list of fitted regression objects (see stargazer manual)
#' @param caption string with table's title
#' @param label string with latex's label
#' @param ... additional options (see stargazer)
#' @export
#'
#' @examples
#' ols.1 <- lm(mpg ~ cyl, mtcars)
#' ols.2 <- update(ols.1, ~ . + gear)
#' ols.3 <- update(ols.1, ~ . + am + I(gear^2))
#' models <- list(baseline=ols.1, gear=ols.2, fe=ols.3)
#' outreg(models, column.labels=names(models)
#'        , caption="Table's caption", label="my label", notes="My notes")
outreg <- function(x, caption=NULL, label=NULL, notes=NULL, tab.layout="-m#c-to-s-", ...) { 
  require(stargazer)
  cat("\\begin{table}\n\\centering\n")
  cat(sprintf("\\caption{%s}\n", caption))
  cat(sprintf("\\label{%s}\n", label))
  stargazer::stargazer(x, header=FALSE, float=FALSE, table.layout=tab.layout, ...)
  if(!is.null(notes)) {
    cat("\\begin{flushleft}\\footnotesize\n")                  
    cat(sprintf("Note: %s\n", notes))
    cat("\\end{flushleft}\n")
  }
  cat("\\end{table}\n")
}
