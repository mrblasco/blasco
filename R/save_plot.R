#' Save plot to file
#' 
#' After having defined a plot function, use this tool to save the plot to file (.png or .pdf). 
#' 
#' @param FUN Function making the plots (e.g., hist)
#' @param file Where to save the figure
#' @param w Figure width in inches
#' @param h Figure height in inches
#' @param view Logical, open the saved file
#' @param ... Arguments passed to the FUN function
#' @export
#' @examples
#' save_plot(hist, x = rnorm(1e3), file = tempfile(fileext = ".png"))
save_plot <- function(FUN, file = NULL, w = 6, h = 6, view = TRUE, ...) {
  if (!is.null(file)) {
    ext <- gsub(".*\\.([^.]*)?$", "\\1", file)
    switch(ext, "png" = png(file, width = w, height = h, units = "in", res = 500)
              , "pdf" = pdf(file, width = w, height = h)
              , stop("Only .png or .pdf extensions are allowed")
              )
    on.exit(dev.off())
    out <- tryCatch(expr = FUN(...)
                  , error = function(e) {
                      message("Error in plot function")
                      stop(e)
                    })
    dev.off()
    on.exit()
    if (view) system(paste("open", file))
  }
}


#my_plot <- function(n, ...) hist(rnorm(n, mean = ifelse(runif(n) < 1/3, 5, 0)), ...)
#save_plot(my_plot, n = 1e3, breaks = 1e2, border = 'white', col = "gray", file = "/tmp/test3.png") 
