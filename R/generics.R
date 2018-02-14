## generic functions

#' @include aaa.R
NULL

#' @title \code{panelPomp} generic
#' @description \code{panelPomp} generic
#' @details This is a generic function: methods can be defined for it.
#' @keywords internal
#' @param object object.
#' @param ... Additional arguments.
#' @export
## basic ppomp constructor
setGeneric(name = "panelPomp",
           def = function(object, ...) standardGeneric("panelPomp"))

#' @title \code{unitobjects} generic.
#' @description \code{unitobjects} generic.
#' @details This is a generic function: methods can be defined for it.
#' @keywords internal
#' @param object object.
#' @param ... Additional arguments.
#' @export
## slot extractors: ppomp class
setGeneric(name = "unitobjects",
           def = function(object, ...) standardGeneric("unitobjects"))

#' @title \code{unitlogLik} generic.
#' @description \code{unitlogLik} generic.
#' @details This is a generic function: methods can be defined for it.
#' @keywords internal
#' @param object object.
#' @param ... Additional arguments.
#' @export
## slot extractors: ppomp class
setGeneric(name = "unitlogLik",
           def = function(object, ...) standardGeneric("unitlogLik"))
