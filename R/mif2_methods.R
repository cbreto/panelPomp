#' @include mif2.R
NULL

## methods for class 'mif2d.ppomp'
##' @rdname mif2
##' 
##' @param object an object resulting from the application of IF2 (i.e., of 
##' class \code{mif2d.ppomp})
##' @param pars names of parameters
##'
##' @return
##' \code{traces} returns the estimated parameter values at different 
##' iterations of the IF2 algorithm in the natural scale. The default is to 
##' return values for all parameters but a subset of parameters can be passed 
##' via the optional argument \code{pars}.
#' @export
setMethod(
  "traces",
  signature=signature(object="mif2d.ppomp"),
  definition = function (object, pars, ...) {
    shmat <- object@pconv.rec
    sparray <- object@pconv.rec.array
    nm <- dimnames(sparray)
    if (!missing(pars)) {
      pars <- as.character(pars)
      shp <- as.character(intersect(pars,colnames(shmat)))
      spp <- as.character(intersect(pars,nm[[2]]))
      bad.pars <- setdiff(pars,c(colnames(shmat),nm[[2]]))
      if (length(bad.pars) > 0) 
        stop("in ", sQuote("traces"),": name(s) ",
             paste(sQuote(bad.pars),collapse = ","),
             " correspond to no parameter(s).",
             call. = FALSE)
      shmat <- shmat[,shp,drop=FALSE]
      sparray <- sparray[,spp,,drop=FALSE]
    }
    dm <- dim(sparray)
    nm <- dimnames(sparray)
    dim(sparray) <- c(dm[1],dm[2]*dm[3])
    dimnames(sparray) <- list(
      iteration=nm[[1]],
      variable=outer(nm[[2]],nm[[3]],sprintf,fmt="%s[%s]")
    )
    mat <- cbind(shmat,sparray)
    names(dimnames(mat)) <- c("iteration","variable")
    mat
  }
)
