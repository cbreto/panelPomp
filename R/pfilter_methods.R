## methods for pfilterd.ppomp objects (other than workhorses: pfilter, mif2, etc.)

#' @include pfilter.R
NULL

#' @rdname pfilter
#' @author Carles Breto
#' @return
#' \code{logLik} returns a \code{numeric} vector.
#' @examples
#' # extract single log likelihood for the entire panel
#' logLik(prw)
#' @export
#'
setMethod(
  "logLik",
  signature = signature(object = "pfilterd.ppomp"),
  definition = function(object,...) object@ploglik
)

#' @rdname pfilter
# @author Carles Breto
#' @return
#' \code{unitlogLik} returns a \code{numeric} vector.
#' @examples
#' # extract log likelihood for each panel unit
#' unitlogLik(prw)
#' @export
#'
setMethod(
  "unitlogLik",
  signature = signature(object = "pfilterd.ppomp"),
  definition = function(object,...) object@unit.logliks
)
