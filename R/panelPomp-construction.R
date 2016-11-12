#' @include panelPomp-methods.R
NULL

#' Construct \code{panelPomp} objects.
#'
#' S4 method.
#'
#' S4 method.
#'
#' @param object a named \code{list} of \code{pomp} objects.
#' @param shared a named \code{numeric vector}.
#' @param specific a \code{matrix} with parameters on named rows and panel units on named columns.
#' @param params optional; a list with (named) 'shared' and 'specific' elements.
#' more than one column.
#'
#' @export
#'
setMethod(
  "panelPomp",
  signature=signature(object="list"),
  definition = function (object, shared = numeric(0),
                         specific = array(numeric(0), dim = c(0,0)),
                         params = list(shared = shared, specific = specific)
  ) {
    ep <- paste0(sQuote("panelPomp::panelPomp")," error: ")
    
    if (class(object[[1]])!="pomp") 
      stop(ep,"The ",sQuote("unit.objects")," slot must be a list of ",
           sQuote("pomp")," objects.",call.=FALSE
      )
    if (!missing(shared) && !missing(specific) && !missing(params)) 
      stop(ep,"specify either ",sQuote("params")," only, ",sQuote("params"),
           " and ",sQuote("shared")," , or ",sQuote("params")," and ",
           sQuote("specific"),".",call.=FALSE
      )
    pParams <- params
    if (!missing(shared)) pParams$shared <- shared
    if (!missing(specific)) pParams$specific <- specific
    pPomp.internal(pompList=object,pParams=pParams)
  }
)
