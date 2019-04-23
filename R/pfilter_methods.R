## methods for pfilterd.ppomp objects (other than workhorses: pfilter, mif2, etc.)

#' @include pfilter.R
NULL

#' @rdname pfilter
#' @export
#'
setMethod(
  "logLik",
  signature = signature(object = "pfilterd.ppomp"),
  definition = function(object,...) object@ploglik
)

#' @rdname pfilter
#' @export
#'
setMethod(
  "unitlogLik",
  signature = signature(object = "pfilterd.ppomp"),
  definition = function(object,...) object@unit.logliks
)
