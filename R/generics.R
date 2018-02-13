## generic functions

#' @include misc-exported.R
NULL

#' \code{panelPomp} generic
#'
#' \code{panelPomp} generic
#'
#' This is a generic function: methods can be defined for it.
#'
#' @keywords internal
#' @param object object.
#' @param ... Additional arguments.
#'
#' @export
## basic ppomp constructor
setGeneric(name = "panelPomp",
           def = function(object, ...) standardGeneric("panelPomp"))

#' \code{unitobjects} generic.
#'
#' \code{unitobjects} generic.
#'
#' This is a generic function: methods can be defined for it.
#'
#' @keywords internal
#' @param object object.
#' @param ... Additional arguments.
#'
#' @export
## slot extractors: ppomp class
setGeneric(name = "unitobjects",
           def = function(object, ...) standardGeneric("unitobjects"))

#' \code{unitlogLik} generic.
#'
#' \code{unitlogLik} generic.
#'
#' This is a generic function: methods can be defined for it.
#'
#' @keywords internal
#' @param object object.
#' @param ... Additional arguments.
#'
#' @export
## slot extractors: ppomp class
setGeneric(name = "unitlogLik",
           def = function(object, ...) standardGeneric("unitlogLik"))
