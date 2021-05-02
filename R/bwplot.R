#  bwplot
#'
#' Scatterplot with black-and-white style
#'
#' @param x Coordinates of points in the plot
#'
#' @param y Coordinates of points in the plot (optional)
#'
#' @param ... Optional graphics arguments
#'
#' @export
#'
#' @return
#' None.
bwplot <- function(x, y=NULL, ..., type="p", hlines=NULL, hlines.col="lightgray", hlines.lty=1, hlines.lwd=1,
             vlines=NULL, vlines.col="lightgray", vlines.lty=1, vlines.lwd=1,
             xat=NULL, yat=NULL, axis.col = "lightgray", bgcolor="gray99",
             pch=21, bg="lightblue", col="black",
             v_over_h=FALSE) {
    if(missing(x) || is.null(x)) stop("x unspecified")

    hidebwplot <- function(x, y, ..., type="p", hlines=NULL, hlines.col, hlines.lty, hlines.lwd,
                 vlines=NULL, vlines.col, vlines.lty, vlines.lwd,
                 xat=pretty(x), yat=pretty(y), bgcolor="gray90", xaxt="n", yaxt="n",
                 col.lab=par("col.lab"),
                 xlim=NULL, ylim=NULL,
                 xlab, ylab, xname, yname,
                 las=1, mgp=c(2.1, 0.5, 0), mgp.x=NULL, mgp.y=NULL,
                 pch=21, bg="lightblue", col="black", axis.col = "lightgray", 
                 v_over_h=FALSE)
        {
            if(is.null(mgp.x)) mgp.x <- mgp # (label location, tick-mark labels, tick marks)
            if(is.null(mgp.y)) mgp.y <- mgp

            if(is.null(y)) {
                if(missing(xlab)) xlab <- "Index"
                if(missing(ylab)) ylab <- xname
                y <- x
                x <- seq(along=x)
            }
            else {
                if(missing(xlab)) xlab <- xname
                if(missing(ylab)) ylab <- yname
            }

            if(is.null(ylim))
                ylim <- range(y, na.rm=TRUE)
            if(is.null(hlines)) {
                if(!is.null(yat))
                    hlines <- yat
                else
                    hlines <- pretty(ylim)
            }
            else if(length(hlines)==1 && is.na(hlines))
                hlines <- NULL

            if(is.null(xlim))
                xlim <- range(x, na.rm=TRUE)
            if(is.null(vlines)) {
                if(!is.null(xat))
                    vlines <- xat
                else
                    vlines <- pretty(xlim)
            }
            else if(length(vlines)==1 && is.na(vlines))
                vlines <- NULL

            # blank plot
            plot(x, y, ..., type="n", xaxt="n", yaxt="n", xlab="", ylab="",
                 xlim=xlim, ylim=ylim, bty = "n")

            # axis titles
            title(xlab=xlab, mgp=mgp.x, col.lab=col.lab)
            title(ylab=ylab, mgp=mgp.y, col.lab=col.lab)

            # add gray rectangle
            u <- par("usr")
            rect(u[1], u[3], u[2], u[4], col=bgcolor, border=NA)

            # x axis: if adding white lines, skip the tick marks and move the numbers closer
            if(!(!is.null(xat) && length(xat)==1 && is.na(xat))) { # if a single NA, skip x-axis
                if(!is.null(xat)) {
                    if(!is.null(vlines))
                        axis(side=1, at=xat, mgp=mgp.x, tick=FALSE, las=las, col = axis.col)
                    else
                        axis(side=1, at=xat, las=las, col = axis.col)
                }
                else {
                    if(!is.null(vlines))
                        axis(side=1, mgp=mgp.x, tick=FALSE, las=las, col = axis.col)
                    else
                        axis(side=1, las=las, col = axis.col)
                }
            }

            # y axis: like the x-axis
            if(!(!is.null(yat) && length(yat)==1 && is.na(yat))) { # if a single NA, skip y-axis
                if(!is.null(yat)) {
                    if(!is.null(hlines))
                        axis(side=2, at=yat, mgp=mgp.y, tick=FALSE, las=las, col = axis.col)
                    else
                        axis(side=2, at=yat, las=las, col = axis.col)
                }
                else {
                    if(!is.null(hlines))
                        axis(side=2, mgp=mgp.y, tick=FALSE, las=las, col = axis.col)
                    else
                        axis(side=2, las=las, col = axis.col)
                }
            }

            if(!is.null(vlines) && !v_over_h)
                abline(v=vlines, col=vlines.col, lty=vlines.lty, lwd=vlines.lwd)
            if(!is.null(hlines))
                abline(h=hlines, col=hlines.col, lty=hlines.lty, lwd=hlines.lwd)
            if(!is.null(vlines) && v_over_h)
                abline(v=vlines, col=vlines.col, lty=vlines.lty, lwd=vlines.lwd)

            points(x, y, ..., pch=pch, bg=bg, col=col, type=type)

            # add black border again
            #abline(v=u[1:2], h=u[3:4])
        }

    hidebwplot(x=x, y=y, ..., type=type, hlines=hlines, hlines.col=hlines.col,
                 hlines.lty=hlines.lty, hlines.lwd=hlines.lwd,
                 vlines=vlines, vlines.col=vlines.col,
                 vlines.lty=vlines.lty, vlines.lwd=vlines.lwd,
                 xat=xat, yat=yat, bgcolor=bgcolor,
                 axis.col = axis.col, 
                 pch=pch, bg=bg, col=col,
                 xname=substitute(x), yname=substitute(y),
                 v_over_h=v_over_h)
    invisible()
}