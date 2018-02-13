## basic constructor of the panelPomp class

#' @include panelPomp_class.R
NULL

pPomp.internal <- function(pompList,pParams,
                           verbose=getOption("verbose",FALSE)) {
  # If needed, fix validity checks on 'pParams$specific'
  if (identical(pParams$specific,array(numeric(0),dim=c(0,0)))) {
    pParams$specific <- structure(
      numeric(0),
      dim=c(0,length(pompList)),
      dimnames=list(NULL,names(pompList))
    )
  }
  new("panelPomp", unit.objects=pompList,pParams=pParams)
}

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
#' @aliases panelPomp-class
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

#' Redefine shared/specific configuration of a panelPomp object.
#'
#' Extract shared and specific parameter values from a panelPomp and use them to create a new panelPomp object with a new configuration of shared and specific parameters.
#'
#' @param object panelPomp object.
#' @param shared character; names of shared parameters. Those which were not originally shared are copied from the specific parameters for the first panel unit. Defaults to \code{NULL}, which implies all parameters are specific.
#'
#' @export
#'
setMethod(
  f = "panelPomp",
  signature = signature(object = "panelPomp"),
  definition = function (object, shared = NULL) {
    parnames <- c(names(coef(object)$shared),row.names(coef(object)$sp))
    stopifnot(all(shared%in%parnames))
    sp <- parnames[!parnames%in%shared]
    # make matrix from coef(object)$sh that can be rbinded to coef(object)$sp
    sh0 <- names(coef(object)$shared)
    not.in.sp0 <- matrix(
      coef(object)$shared,nrow=length(sh0),ncol=length(object),
      dimnames=list(sh0,names(object))
    )
    all.sp <- rbind(coef(object)$specific,not.in.sp0)
    stopifnot(!as.logical(anyDuplicated(row.names(all.sp))))
    # make vector from coef(object)$sp[1,] that can be c()d to coef(object)$sh
    all.sh <- c(coef(object)$shared,coef(object)$specific[,1])
    shs <- all.sh[shared]
    sps <- all.sp[sp,]
    panelPomp(as(object,"list"),shared=shs,specific=sps)
  }
)
