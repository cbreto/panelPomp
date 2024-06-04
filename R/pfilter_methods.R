## methods for pfilterd.ppomp objects (other than workhorses: pfilter, mif2, etc.)

#' @include pfilter.R
NULL

#' @rdname pfilter
#' @author Carles \Breto
#' @return
#' When applied to an object of class \code{pfilterd.ppomp}, \code{logLik()}
#' returns a \code{numeric} value.
#' @examples
#' # extract single log likelihood for the entire panel
#' logLik(pfrw)
#' @export
setMethod(
  "logLik",
  signature = signature(object = "pfilterd.ppomp"),
  definition = function(object,...) object@ploglik
)

#' @rdname pfilter
# @author Carles \Breto
#' @return \unitLogLikReturn
# \unitLoglikReturn is resused in documentation of generic function introduced by the panelPomp package
#' @example examples/unitLogLik.R
#' @export
setMethod(
  "unitLogLik",
  signature = signature(object = "pfilterd.ppomp"),
  definition = function(object,...) object@unit.logliks
)

#' Extract Unit Log-Likelihoods
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' @param object an object for which log likelihood values for units can be extracted.
#' @param ... additional arguments.
#' @return When given objects of class \code{pfilterd.ppomp}, \code{unitloglik()} returns a \code{numeric} vector.
#' @examples
#' # filter, which generates log likelihoods
#' pfrw <- pfilter(panelRandomWalk(),Np=10)
#'
#' # extract log likelihood for each panel unit
#' unitlogLik(pfrw)
#'
#' @export
setMethod(
  "unitlogLik",
  signature = signature(object = "pfilterd.ppomp"),
  definition = function(object,...) {
    lifecycle::deprecate_warn("1.2.0", "unitlogLik()", "unitLogLik()")
    object@unit.logliks
  }
)
