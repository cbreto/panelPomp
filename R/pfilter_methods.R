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
#' @return \unitlogLikReturn
# \unitloglikReturn is resused in documentation of generic function introduced by the panelPomp package
#' @example examples/unitlogLik.R
#' @export
setMethod(
  "unitlogLik",
  signature = signature(object = "pfilterd.ppomp"),
  definition = function(object,...) object@unit.logliks
)
