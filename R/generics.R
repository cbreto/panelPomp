## generic functions

#' @include aaa.R
NULL

#' @title \code{pParams} generic.
#' @description \code{pParams} generic function.
#' @details This is a generic function: methods can be defined for it.
#' @keywords internal
#' @param object object.
#' @param ... Additional arguments.
#' @export
setGeneric(name = "pparams",
           def = function(object, ...) standardGeneric("pparams"))

#' @title \code{unitobjects} generic.
#' @description \code{unitobjects} generic.
#' @details This is a generic function: methods can be defined for it.
#' @keywords internal
#' @param object object.
#' @param ... Additional arguments.
#' @export
setGeneric(name = "unitobjects",
           def = function(object, ...) standardGeneric("unitobjects"))

#' @title \code{unitlogLik} generic.
#' @description \code{unitlogLik} generic.
#' @details This is a generic function: methods can be defined for it.
#' @keywords internal
#' @param object object.
#' @param ... Additional arguments.
#' @export
setGeneric(name = "unitlogLik",
           def = function(object, ...) standardGeneric("unitlogLik"))
