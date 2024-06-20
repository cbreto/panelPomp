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
# @author Carles \Breto, Jesse Wheeler
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

#' Modifying parameters of filtered objects
#'
#' The setter functions for parameters of \code{pfilterd.ppomp} objects
#' do not allow users to set parameters of \code{panelPomp} objects that
#' have been filtered. This is done to avoid the possibility of
#' having parameter values in an object that do not match other
#' attributes of a filtered object to be saved together.
#' @param object \code{pfilterd.ppomp} object
#' @param value New parameter value. This function does not allow
#'  users to set this value.
#' @param ... additional arguments.
#' @export
#' @rdname pfilterd-setter
setMethod(
  "coef<-",
  signature=signature(object="pfilterd.ppomp"),
  definition=function (object, ..., value) {
    ## check names(value)
    ep <- wQuotes("in ''coef<-'': ")
    stop(wQuotes(ep,"cannot change parameters of a filtered object","."),call.=FALSE)
  }
)

#' Modifying parameters of filtered objects
#'
#' The setter functions for parameters of \code{pfilterd.ppomp} objects
#' do not allow users to set parameters of \code{panelPomp} objects that
#' have been filtered. This is done to avoid the possibility of
#' having parameter values in an object that do not match other
#' attributes of a filtered object to be saved together.
#' @param object \code{pfilterd.ppomp} object
#' @param value New parameter value. This function does not allow
#'  users to set this value.
#' @export
#' @rdname pfilterd-setter
setMethod(
  "shared<-",
  signature=signature(object="pfilterd.ppomp"),
  definition=function (object, value) {
    ## check names(value)
    ep <- wQuotes("in ''shared<-'': ")
    stop(wQuotes(ep,"cannot change parameters of a filtered object","."),call.=FALSE)
  }
)

#' Modifying parameters of filtered objects
#'
#' The setter functions for parameters of \code{pfilterd.ppomp} objects
#' do not allow users to set parameters of \code{panelPomp} objects that
#' have been filtered. This is done to avoid the possibility of
#' having parameter values in an object that do not match other
#' attributes of a filtered object to be saved together.
#' @param object \code{pfilterd.ppomp} object
#' @param value New parameter value. This function does not allow
#'  users to set this value.
#' @export
#' @rdname pfilterd-setter
setMethod(
  "specific<-",
  signature=signature(object="pfilterd.ppomp"),
  definition=function (object, value) {
    ## check names(value)
    ep <- wQuotes("in ''specific<-'': ")
    stop(wQuotes(ep,"cannot change parameters of a filtered object","."),call.=FALSE)
  }
)
