## methods for pfilterd.ppomp objects (other than workhorses: pfilter, mif2, etc.)

#' @include pfilter.R
NULL

#' @rdname pfilter
#' @author Carles Breto
#' @return
#' \code{panelPomp} object, including a list of \code{pfilterd_pomp} objects.
#' @examples
#' fprw <- pfilter(panelRandomWalk(),Np=10)
#' fprw
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
#' A \code{numeric} vector.
#' @examples
#' unitlogLik(fprw)
#' @export
#'
setMethod(
  "unitlogLik",
  signature = signature(object = "pfilterd.ppomp"),
  definition = function(object,...) object@unit.logliks
)
