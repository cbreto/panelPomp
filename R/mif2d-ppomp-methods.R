#' @include mif2-internal-functions.R
NULL

# methods for class 'mif2d.ppomp'

#' Apply the \code{mif2} algorithm to \code{mif2d.ppomp} objects.
#'
#' S4 method
#'
#' S4 method
#' 
#' @inheritParams pomp::mif2
#' @inheritParams mif2,panelPomp-method 
#' @param cooling.fraction.50 cooling.fraction.50 (seems to cause an error if documentation inherited from 'pomp' package)
#' @param transform transform (seems to cause an error if documentation inherited from 'pomp' package)
#' @param ptol Unit-specific tolerances can be passed as a named numeric vector with names matching \code{names(unitobjects(object))}.
#' @param ... ...
#'
#' @export
#'
setMethod(
  f = "mif2",
  signature = signature(object = "mif2d.ppomp"),
  definition = function (object,
                         Nmif,
                         shared.start,
                         specific.start,
                         Np,
                         prw.sd,
                         transform,
                         cooling.type,
                         cooling.fraction.50,
                         ptol,
                         ...) {
    if (missing(Nmif))
      Nmif <- object@Nmif
    if (missing(shared.start))
      shared.start <- coef(object)$shared
    if (missing(specific.start))
      specific.start <- coef(object)$specific
    if (missing(Np))
      Np <- object@Np    
    if (missing(prw.sd))
      prw.sd <- object@prw.sd
    if (missing(transform))
      transform <- object@transform
    if (missing(cooling.type))
      cooling.type <- object@cooling.type
    if (missing(cooling.fraction.50))
      cooling.fraction.50 <- object@cooling.fraction.50
    if (missing(ptol))
      ptol <- object@ptol
    
    f <- selectMethod(f = "mif2", signature = "panelPomp")
    f(
      object = object,
      shared.start = shared.start, 
      specific.start = specific.start,
      Np = Np,
      Nmif = Nmif,      
      cooling.type = cooling.type,
      cooling.fraction.50 = cooling.fraction.50,
      transform = transform,
      prw.sd = prw.sd,
      ptol = ptol,
      ...
    )
  }
)