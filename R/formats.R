#' Output formats for rmarkdown documents
#'
#' Wrapper for \code{\link[bookdown]{pdf_document2}} to
#' produce documents in my preferred style.
#'
#' @param \dots Arguments passed to \code{\link[bookdown]{pdf_document2}}.
#'
#' @return An R Markdown output format object.
#'
#' @author Andrea Blasco
#'
#' @export
wp <- function(...) {
  template <- system.file("rmarkdown/templates/article.tex", package="blasco")
  bookdown::pdf_document2(..., template = template)
}

#' @rdname wp
#' @export
boyd <- function(...) {
  template <- system.file("rmarkdown/templates/boydbeamer.tex", package="blasco")
  bookdown::beamer_presentation2(..., template = template)
}
