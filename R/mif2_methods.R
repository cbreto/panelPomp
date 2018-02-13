#' @include mif2.R
NULL

# methods for class 'mif2d.ppomp'

#' IF2: Maximum likelihood by iterated, perturbed Bayes maps
#'
#' Extension to panel models of the improved iterated filtering algorithm (Ionides et al., 2015) for estimating parameters of a partially observed Markov process.
#' Iterated filtering algorithms rely on extending a partially observed Markov process model of interest by introducing random perturbations to the model parameters. 
#' The space where the original parameters live is then explored at each iteration by running a particle filter.
#' Convergence to a maximum likelihood estimate has been established for appropriately constructed procedures that iterate this search over the parameter space while diminishing the intensity of perturbations (Ionides et al. 2006, 2011, 2015).
#' 
#' @inheritParams pomp::mif2
#' @inheritParams mif2,panelPomp-method 
#' @param transform logical; if TRUE, optimization is performed on the estimation scale (see \code{pomp} documentation).
#'
#' @export
#'
setMethod(
  "mif2",
  signature=signature(object="mif2d.ppomp"),
  definition = function (object, Nmif, shared.start, specific.start, Np, rw.sd,
                         transform, cooling.type, cooling.fraction.50, tol,
                         ...) {
    if (missing(Nmif)) Nmif <- object@Nmif
    if (missing(shared.start)) shared.start <- coef(object)$shared
    if (missing(specific.start)) specific.start <- coef(object)$specific
    if (missing(Np)) Np <- object@Np    
    if (missing(rw.sd)) rw.sd <- object@prw.sd
    if (missing(transform)) transform <- object@transform
    if (missing(cooling.type)) cooling.type <- object@cooling.type
    if (missing(cooling.fraction.50)) cooling.fraction.50 <- object@cooling.fraction.50
    if (missing(tol)) tol <- object@tol
    
    f <- selectMethod("mif2",signature="panelPomp")
    f(object=object,shared.start=shared.start,specific.start=specific.start,
      Np=Np,Nmif=Nmif,cooling.type=cooling.type,
      cooling.fraction.50=cooling.fraction.50,transform=transform,rw.sd=rw.sd,
      tol=tol,...)
  }
)
