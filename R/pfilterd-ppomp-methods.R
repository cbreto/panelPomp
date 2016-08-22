#' @include pfilter-internal.R
NULL

#' Extract loglikelihood from \code{pfilterd.ppomp} object
#'
#' Now then, now then, what's all this?
#'
#' S4 method
#'
#' @param object object
#' @param ... Additional arguments
#'
#' @export
#'
setMethod(
  "logLik",
  signature = signature(object = "pfilterd.ppomp"),
  definition = function(object,...) object@ploglik
)# END setMethod

#' Extract unit loglikelihoods from \code{pfilterd.ppomp} object
#'
#' S4 method
#'
#' S4 method
#'
#' @param object object
#' @param ... Additional arguments
#'
#' @export
#'
setMethod(
  "unitlogLik",
  signature = signature(object = "pfilterd.ppomp"),
  definition = function(object,...) object@unit.logliks
)# END setMethod
