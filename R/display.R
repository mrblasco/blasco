# From ARM 
if (!isGeneric("display")) {
    setGeneric("display",
               function(object, ...)
               standardGeneric("display"))
}

# Method for lm objects
#
#' @export
setMethod("display", signature(object = "lm"),
    function(object, digits=2, detail=FALSE, omit=NULL)
    {
    out <- NULL
    out$call <- object$call
    summ <- summary (object)
    out$sigma.hat <- summ$sigma
    out$r.squared <- summ$r.squared
    if(detail){
      coef <- summ$coef[,,drop=FALSE]
    }
    else{
      coef <- summ$coef[,1:2,drop=FALSE]
    }
    dimnames(coef)[[2]][1:2] <- c("coef.est","coef.se")
    out$coef <- coef[,"coef.est"]#,drop=FALSE]
    out$se <- coef[,"coef.se"]#,drop=FALSE]
    out$t.value <- summ$coef[,3]
    out$p.value <- summ$coef[,4]
    out$n <- summ$df[1] + summ$df[2]
    out$k <- summ$df[1]
    print (out$call)
    if(is.null(omit)) {
      print(round (coef, digits))
    } else {
      omitted <- grepl(omit,rownames(coef))
      print(round (coef[!omitted,], digits))
      cat("...",fill=T)
      cat("(omitted",sum(omitted),"vars)",fill=T)
    }
    cat("---\n")
    cat (paste ("n = ", out$n, ", k = ", out$k,
    "\nresidual sd = ", round (out$sigma.hat, digits),
    ", R-Squared = ", round (out$r.squared, 2), "\n", sep=""))
    return(invisible(out))
    }
)


#' @export
setMethod("display", signature(object = "glm"),
    function(object, digits=2, detail=FALSE, omit=NULL) {
    out <- NULL
    out$call <- object$call
    summ <- summary(object, dispersion = object$dispersion)
    if(detail){
      coef <- summ$coef[, , drop = FALSE]
      out$z.value <- coef[,3]#,drop=FALSE]
      out$p.value <- coef[,4]#,drop=FALSE]
    }
    else{
      coef <- summ$coef[, 1:2, drop = FALSE]
    }
    dimnames(coef)[[2]][1:2] <- c("coef.est", "coef.se")
    out$n <- summ$df[1] + summ$df[2]
    out$k <- summ$df[1]
    out$coef <- coef[,"coef.est"]
    out$se <- coef[,"coef.se"]
    print(out$call)
    if(is.null(omit)) {
      print(round (coef, digits))
    } else {
      omitted <- grepl(omit,rownames(coef))
      print(round (coef[!omitted,], digits))
      cat("...",fill=T)
      cat("(omitted",sum(omitted),"vars)",fill=T)
    }
    out$deviance <- summ$deviance
    out$null.deviance <- summ$null.deviance
    cat("---\n")
    cat(paste("  n = ", out$n, ", k = ", out$k, "\n  residual deviance = ",
        round(out$deviance, 1), ", null deviance = ", round(out$null.deviance, 1), " (difference = ", round(summ$null.deviance - summ$deviance, 1), ")", "\n", sep = ""))
    out$dispersion <- if (is.null(object$dispersion)){
                        summ$dispersion
                      } else {
                        object$dispersion
                      }
    if (out$dispersion != 1) {
      cat(paste("  overdispersion parameter = ", round(out$dispersion, 1), "\n", sep = ""))
      if (family(object)$family=="gaussian") {
        out$sigma.hat <- sqrt(out$dispersion)
        cat(paste("  residual sd is sqrt(overdispersion) = ",
                  round(out$sigma.hat, digits), "\n", sep = ""))
      }
    }
    return(invisible(out))
  }
)

