#' Preview table in PDF using pandoc
#'
#' Converts a table in latex into a PDF document (class standalone)
#'
#' @param x vector of well-formatted latex table (e.g., from kable)
#' @param filename name of destination file (optional)
#' @param pandoc.opt character with pandoc options (-V <variables> and -H <header>)
#' @export
#'
#' @examples
#' tab <- xtabs( ~ cyl + vs, data = mtcars) 
#' preview_latex(knitr::kable(tab, format = "latex"))
preview_latex <- function(x, filename = NULL, pandoc.opt = NULL, header=NULL) {
  if (is.null(filename)) filename <- tempfile(fileext = ".md")
  cat(x, file = filename, fill = T)
  if (!is.null(header)) {
    cat(sprintf("\n\n---\nheader-includes: %s\n---\n\n", header), file = filename, fill = T, append = TRUE)
  }
  if (is.null(pandoc.opt)) {
    pandoc.opt <- "-V documentclass:standalone -V geometry:margin=1in,landscape"
  }
  pand.cmd <- trimws(paste("pandoc", sprintf("%s", pandoc.opt)))
  system(paste(pand.cmd, filename, "-o", gsub(".md", ".pdf", filename)))
  system(paste("open", gsub(".md", ".pdf", filename)))
  return(x)
}
