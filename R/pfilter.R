## particle filtering codes

#' @include params.R
NULL

#' @title Particle filtering for panel data
#' @description Tools for applying particle filtering algorithms to panel data.
#' @param data An object of class \code{panelPomp} or inheriting class 
#' \code{panelPomp}.
#' @name pfilter
#' @section Methods:
#' \describe{
#'   \item{logLik}{Extracts the estimated log likelihood for the entire panel.}
#'   \item{unitlogLik}{Extracts the estimated log likelihood for each panel unit.}
#'   }
#' @references \arulampalam2002
#' 
#' \breto2018
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
    stop(wQuotes("in ''pfilter'': ''tol'' must be a single positive scalar or",
                 " a vector of length ",U),call.=FALSE)
  }
  for (i.u in 1:U) {
    pfilterd.pomp.list[[i.u]] <-
      pomp::pfilter(
        object@unit.objects[[i.u]],
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
    shared = params$shared,
    specific = params$specific,
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
  signature=signature(data="panelPomp"),
  definition = function(data, shared, specific, params, Np, 
                        tol = 1e-17,
                        verbose = getOption("verbose"),
                        ...) {
    object <- data # the argument name 'data' is fixed by pomp's generic
    ep <- wQuotes("in ''pfilter'': ")
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
      else shared <- object@shared
    } 
    if (missing(specific)){
      if (!missing(params)) specific <- params$specific 
      else specific <- object@specific
    }
    
    # This causes an unintended stop in panelPomp objects that genuinely have 
    # no shared parameters      
    #if (identical(shared,numeric(0))) {
    #  stop(ep,"if ",sQuote("object@shared")," is empty, shared 
    #       parameters must be specified in either ",sQuote("shared"),
    #       " or as part of ",sQuote("params"),".",call.=FALSE
    #  )
    #}
    # Obsolete check: valid panelPomps won't have completely empty sp matrix
    #if (identical(specific,array(numeric(0),dim=c(0,0)))) {
    #  stop(ep,"if ",sQuote("object@specific")," is empty, specific 
    #       parameters must be specified in either ",sQuote("specific"),
    #       " or as part of ",sQuote("params"),".",call.=FALSE
    #  )
    #}
    # if the pParams slot is not empty, check that the shared and specific 
    # structure of any provided starting values match the pParams slot
    if (!is.null(object@shared)){
      if (
        !identical(
          character(0),
          setdiff(names(object@shared),names(shared))
        )
        &
        !(is.null(names(object@shared))&is.null(names(shared)))
      ) {
        stop(ep, "names of ", sQuote("shared"), " must match those of ", 
             sQuote("object@shared"),".", call.=FALSE
        )
      }
    }
    if (!is.null(object@specific)){
      if (
        !identical(
          character(0),
          setdiff(rownames(object@specific),
                  rownames(specific))
        )
        &
        !(
          is.null(rownames(object@specific))
          &
          is.null(rownames(specific))
        )
      ) {
        stop(ep,"rownames of ",sQuote("specific")," must match those of ", 
             sQuote("object@specific"),".",call.=FALSE
        )
      }
      if (!setequal(x = colnames(object@specific), y = colnames(specific))){
        stop(ep, "colnames of ", sQuote("specific"), " must be identical to those of ", 
             sQuote("object@specific"),".", call.=FALSE
        )
      }
    }
    if (missing(Np)) stop(wQuotes(ep,"Missing ''Np'' argument."),call.=FALSE)
    
    pfilter.internal(
      object = object, # internally, 'object' is used, not 'data'
      params = list(shared = shared, specific = specific),
      Np = Np,
      tol = tol,
      verbose = verbose,
      ...
    )
  }
)
