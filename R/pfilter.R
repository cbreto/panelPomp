## particle filtering codes

#' @include panelPomp_methods.R
NULL

#' @title Particle filtering for panel data
#' @description Tools for applying particle filtering algorithms to panel data.
#' @param object An object of class \code{panelPomp} or inheriting class 
#' \code{panelPomp}.
#' @name pfilter
#' @section Methods:
#' \describe{
#'   \item{logLik}{Extracts the estimated log likelihood for the entire panel.}
#'   \item{unitlogLik}{Extracts the estimated log likelihood for each panel unit.}
#'   }
#' @references \arulampalam2002
#' 
#' \breto2017
#' @family panelPomp workhorse functions
#' @seealso \pkg{pomp}'s pfilter at \link[=pfilter,pomp-method]{pfilter}, \link{panel_loglik}
NULL

#' @rdname pfilter
#' @export
setClass(
  'pfilterd.ppomp',
  contains = 'panelPomp',
  slots = c(
    Np = 'numeric',
    ploglik = 'numeric',
    tol = 'numeric',
    unit.logliks = 'numeric'
  ),
  prototype = prototype(
    Np = as.integer(NA),
    ploglik = as.double(NA),
    tol = as.double(NA),
    unit.logliks = numeric(0)
  )
)

# pPfilter algorithm internal functions
pfilter.internal <- function(object, params, Np, 
                             tol, verbose = FALSE, ...) {
  # Turn params list into a matrix
  matrixpParams <- toMatrixPparams(params)
  U <- length(object)
  pfilterd.pomp.list <- setNames(vector(mode="list",length=U),
                                 names(unitobjects(object)))
  if (length(tol)==1) {
    tol <- setNames(rep(tol,U),names(unitobjects(object)))
  } else if (length(tol) != U) {
    stop(sQuotes("in 'pfilter': 'tol' must be a single positive scalar or a ",
                 "vector of length ",U),call.=FALSE)
  }
  for (i.u in 1:U) {
    pfilterd.pomp.list[[i.u]] <-
      pomp::pfilter(
        object = object@unit.objects[[i.u]],
        params = matrixpParams[, i.u],
        Np = Np,
        tol = unname(tol[i.u]),
        ...
      )
  }
  pfilter.internal.unit.logliks <- sapply(pfilterd.pomp.list,logLik)
  pfilter.internal.loglik <- sum(pfilter.internal.unit.logliks)
  new(
    Class = "pfilterd.ppomp",
    unit.objects = pfilterd.pomp.list,
    pParams = params,
    ploglik = pfilter.internal.loglik,
    unit.logliks = pfilter.internal.unit.logliks,
    tol = tol
  )
}

#' @rdname pfilter
#' @inheritParams coef,panelPomp-method 
#' @inheritParams panelPomp
#' @inheritParams pomp::mif2
#' @param tol filtering tolerance for all units.
#' @param ... additional arguments, passed to the \code{pfilter} method of \pkg{pomp}.
#' @export
#'
setMethod(
  "pfilter",
  signature=signature(object="panelPomp"),
  definition = function(object, shared, specific, params, Np, 
                        tol = 1e-17,
                        verbose = getOption("verbose"),
                        ...) {
    ep <- sQuotes("'panelPomp::pfilter' error: ")
    ## check for params format
    if (!missing(params) && is.numeric(params)) params <- pParams(params)

    if (!missing(shared) && !missing(specific) && !missing(params)) 
      stop(ep,"specify either ",sQuote("params")," only, ",sQuote("params"),
           " and ",sQuote("shared")," , or ",sQuote("params")," and ",
           sQuote("specific"),".",call.=FALSE
      )
    
    # Get starting parameter values from 'object,' 'start,' or 'params'
    if (missing(shared)){
      if (!missing(params)) shared <- params$shared 
      else shared <- object@pParams$shared
    } 
    if (missing(specific)){
      if (!missing(params)) specific <- params$specific 
      else specific <- object@pParams$specific
    }
    
    # This causes an unintended stop in panelPomp objects that genuinely have 
    # no shared parameters      
    #if (identical(shared,numeric(0))) {
    #  stop(ep,"if ",sQuote("object@pParams$shared")," is empty, shared 
    #       parameters must be specified in either ",sQuote("shared"),
    #       " or as part of ",sQuote("params"),".",call.=FALSE
    #  )
    #}
    # Obsolete check: valid panelPomps won't have completely empty sp matrix
    #if (identical(specific,array(numeric(0),dim=c(0,0)))) {
    #  stop(ep,"if ",sQuote("object@pParams$specific")," is empty, specific 
    #       parameters must be specified in either ",sQuote("specific"),
    #       " or as part of ",sQuote("params"),".",call.=FALSE
    #  )
    #}
    # if the pParams slot is not empty, check that the shared and specific 
    # structure of any provided starting values match the pParams slot
    if (!is.null(object@pParams$shared)){
      if (
        !identical(
          character(0),
          setdiff(names(object@pParams$shared),names(shared))
        )
        &
        !(is.null(names(object@pParams$shared))&is.null(names(shared)))
      ) {
        stop(ep, "names of ", sQuote("shared"), " must match those of ", 
             sQuote("object@pParams$shared"),".", call.=FALSE
        )
      }
    }
    if (!is.null(object@pParams$specific)){
      if (
        !identical(
          character(0),
          setdiff(rownames(object@pParams$specific),
                  rownames(specific))
        )
        &
        !(
          is.null(rownames(object@pParams$specific))
          &
          is.null(rownames(specific))
        )
      ) {
        stop(ep,"rownames of ",sQuote("specific")," must match those of ", 
             sQuote("object@pParams$specific"),".",call.=FALSE
        )
      }
      if (!identical(x = colnames(object@pParams$specific), y = colnames(specific))){
        stop(ep, "colnames of ", sQuote("specific"), " must be identical to those of ", 
             sQuote("object@pParams$specific"),".", call.=FALSE
        )
      }
    }
    if (missing(Np)) stop(ep,"Missing 'Np' argument.")
    # Check that all parameters in the pomp objects have been provided either as shared or specific ...
    if(!all(names(coef(unitobjects(object)[[1]])) %in% c(names(shared), rownames(specific)))) 
      stop(ep, "At least one 'pomp' parameter needs to be added to the (shared. or specific.) 
           start argument.")
    # ... and viceversa.
    if(!all(c(names(shared), rownames(specific))  %in% names(coef(unitobjects(object)[[1]]))))
      stop(ep, "At least one parameter in the (shared. or specific.) start argument is not 
           being used.")
    
    pfilter.internal(
      object = object,
      params = list(shared = shared, specific = specific),
      Np = Np,
      tol = tol,
      verbose = verbose,
      ...
    )
  }
)
