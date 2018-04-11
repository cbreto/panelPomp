## panelPomp: class and constructor

#' @include panel_logmeanexp.R
NULL

#' @title Constructing \code{panelPomp} objects
#' @name panelPomp
#' @description This function constructs \code{panelPomp} objects, representing 
#' PanelPOMP models (as defined in Breto et al., 2018). PanelPOMP models 
#' involve multiple units, each of which can in turn be modeled by a POMP 
#' model. Such POMP models can be encoded as a \code{list} of \code{pomp} 
#' objects, a cornerstone that the \code{panelPomp} function can use to 
#' construct the corresponding \code{panelPomp} object.
#' @param object required; either (i) a \code{list} of \code{pomp} objects; or 
#' (ii) an object of class \code{panelPomp} or inheriting class 
#' \code{panelPomp}.
#' 
#' If \code{object} is a \code{list} of \code{pomp}s, the list must be named. 
#' All these \code{pomp}s must either have no parameters or have the same 
#' parameter names. (This is just a format requirement. \code{pomp} codes can 
#' ignore any parameter that is irrelevant to any given panel unit.)
#' 
#' If \code{object} is a \code{panelPomp} object, the function allows modifying 
#' the shared and unit-specific configuration of \code{object}.
#' @param shared,specific optional; these arguments depend on the type 
#' of \code{object}.
#' 
#' If \code{object} is a \code{list} of \code{pomp}s, \code{shared} must be a 
#' numeric vector specifying parameter values shared among panel units.
#' \code{specific} must be a \code{matrix} with parameter values that are 
#' unit-specific with rows naming parameters and columns naming units (these 
#' names must match those of \code{object}). If no values are specified and 
#' \code{object} has parameter values, these are set to be all unit-specific.
#' 
#' If \code{object} is a \code{panelPomp} object, these arguments can still be 
#' used as described above to modify the parameters of \code{object}. 
#' Alternatively, the parameter configuration of \code{object} can be modified 
#' providing only a character \code{shared} naming parameters of \code{object} 
#' that should be shared (with values for parameters not originally shared 
#' taken from the unit-specific parameters of the first panel unit of 
#' \code{object}). \code{shared=NULL} sets all parameters as unit-specific.
#' @param params optional; a list with elements named 'shared' and 'specific' 
#' to be passed as \code{shared} and \code{specific} arguments. Alternatively, 
#' a numeric vector. In this case, the nature of parameters is determined via 
#' a naming convention: names ending in \dQuote{\code{[unit_name]}} are assumed 
#' to specify unit-specific parameters; all other names specify shared 
#' parameters.
#' @references \breto2018
#' 
#' \king2015
#' @family panelPomp workhorse functions
#' @seealso \pkg{pomp}'s constructor at \link[=pomp]{pomp}
NULL

#' @rdname panelPomp
#' @export
setClass(
  'panelPomp',
  slots=c(
    unit.objects = 'list',
    pParams = 'list'
  ),
  prototype=prototype(
    unit.objects=list(),
    pParams=list(shared=numeric(),specific=array(numeric(),dim=c(0,0)))
  ),
  validity=function (object) {
    retval <- character(0)
    if (length(object@unit.objects)<1) {
      retval <- append(
        retval,wQuotes("''unit.objects'' is a required argument"))
    } else {
      if (is.null(names(object@unit.objects))) {
        retval <- append(retval,wQuotes("''unit.object'' must have names"))
      } else {
        if (!all(sapply(object@unit.objects,is,"pomp"))) {
          retval <- append(
            retval,
            wQuotes("''unit.objects'' must be a list of ''pomp'' objects"))
        } else {
          coefnms <- lapply(lapply(lapply(object@unit.objects,coef),names),
                            function (le) if (is.null(le)) NULL else sort(le))
          if (!all(sapply(coefnms,identical,coefnms[[1]]))) 
            retval <- append(
              retval,
              wQuotes("The parameter names of all ''pomp'' objects in ",
                      "''unit.objects'' must be the same (albeit ''pomp'' codes",
                      "can ignore parameters that are irrelevant to any given ",
                      "unit)"))
        }
      }
    }
    ## check that optional arguments have the required format
    if (!identical(new("panelPomp")@pParams$specific,object@pParams$specific)) {
      if (!is.list(object@pParams)) {
        retval <- append(retval,wQuotes("''pParams'' must be a list"))
      } else {
        if (!(length(object@pParams) == 2)) {
          retval <-
            append(retval,wQuotes("''pParams'' must be of length two"))
        } else {
          right.list.structure <-
            any(all(sapply(object@pParams,class) == c('numeric','matrix'))
                &
                  all(names(object@pParams) == c("shared","specific")),
                all(sapply(object@pParams,class) == c('matrix','numeric'))
                &
                  all(names(object@pParams) == c("specific","shared")))
          if (!right.list.structure) {
            retval <- append(
              retval,
              wQuotes("The elements of ''pParams'' must be a numeric vector ", 
                      "named ''shared'' and a matrix named ''specific''"))
          } else {
            if (!identical(colnames(object@pParams$specific),
                           names(object@unit.objects))) 
              retval <- append(
                retval,
                wQuotes("The names of columns of the ''specific'' matrix in",
                        "''pParams'' must match those of the list in the ",
                        "''unit.object'' slot"))
            pPnms <-c(names(object@pParams$shared),
                      rownames(object@pParams$specific))
            if (!is.null(pPnms)) pPnms <- sort(pPnms)
            if (!identical(pPnms,coefnms[[1]])) retval <- append(
              retval,
              wQuotes("The parameter names of ''pomp'' objects in ",
                      "''unit.objects'' must match those in the ",
                      "''pParams'' slot"))
          }
        }
      }
    }
    if (length(retval)==0) TRUE else {
      append(retval," (validity check)")
      retval
    }
  }
)

panelPomp.internal <- function(pompList,pParams,
                               verbose=getOption("verbose",FALSE)) {
  ## If needed, fix validity checks on 'pParams$specific'
  if (
    identical(pParams$specific,new("panelPomp")@pParams$specific) &&
    !all(sapply(lapply(pompList,coef),identical,coef(new("pomp"))))
    ) {
    pParams$specific <- array(
      numeric(),
      dim=c(0,length(pompList)),
      dimnames=list(param=character(),unit=names(pompList))
    )
  }
  new("panelPomp",unit.objects=pompList,pParams=pParams)
}

#' @rdname panelPomp
#' @export
panelPomp <- function (object, shared = new("panelPomp")@pParams$shared,
                       specific = new("panelPomp")@pParams$specific,
                       params = list(shared = shared, specific = specific)) {
  ep <- wQuotes("in ''panelPomp'': ")
  if (missing(object)) 
    stop(wQuotes(ep,"''object'' is a required argument."),call.=FALSE)
  if (!missing(shared) && !missing(specific) && !missing(params)) 
    stop(wQuotes(ep,"do not specify all of ''params'', ''shared'' and ",
                 "''specific''."),call.=FALSE)
  
  if (is(object,"panelPomp")) {
    ## if character 'shared': make them shared and make the rest specific
    ## if NULL 'shared': make all specific
    if ((!missing(shared) && (is.character(shared))) || is.null(shared)) {
      if (!missing(specific) || !missing(params)) 
        stop(wQuotes(ep,"if ''shared'' is a character vector (or NULL), unit ",
                     "specific parameters are taken from ''object''."),
             call.=FALSE)
      ## modify the panelPomp object
      parnames <- c(names(object@pParams$shared),row.names(object@pParams$sp))
      stopifnot(all(shared%in%parnames))
      sp <- parnames[!parnames%in%shared]
      ## make matrix from object@pParams$sh to be rbind()d to object@pParams$sp
      sh0 <- names(object@pParams$shared)
      not.in.sp0 <- matrix(
        object@pParams$shared,nrow=length(sh0),ncol=length(object),
        dimnames=list(sh0,names(object))
      )
      all.sp <- rbind(object@pParams$specific,not.in.sp0)
      stopifnot(!as.logical(anyDuplicated(row.names(all.sp))))
      ## make vector from object@pParams$sp[1,] to be c()d to object@pParams$sh
      all.sh <- c(object@pParams$shared,object@pParams$specific[,1])
      pParams <- list(shared=all.sh[shared],specific=all.sp[sp,])
      ## make sure pParams is a valid slot
      if (is.null(shared)) pParams$shared <- numeric()
    } else { ## not using character shared
      ## check for params format
      if (is.numeric(params)) params <- pParams(params)
      pParams <- params
      if (!missing(shared)) pParams$shared <- shared
      if (!missing(specific)) pParams$specific <- specific
    }
  } else {## is(object,"pompList")) should be TRUE!
    if (!all(sapply(as(object,"list"),is,"pomp"))) 
      stop(wQuotes(ep,"''object'' must be a either a ''panelPomp'' object or a",
                   " list of ''pomp'' objects."),
           call.=FALSE)
    ## construct a panelPomp
    ## if no parameters provided, ...
    if (missing(shared) && missing(specific) && missing(params)) {
      ## check for missing params ...
      MISS <- sapply(lapply(object,coef),identical,coef(new("pomp")))
      if (any(MISS)) {
        if (all(MISS)) {
          pParams <- new("panelPomp")@pParams
        } else {
          stop(ep,wQuotes("the parameter names of all ''pomp'' objects ",
                          "must be the same (albeit ''pomp'' codes can ignore ",
                          "parameters that are irrelevant to any given unit)"),
               call.=FALSE)}
      } else ## ... and make all unit-specific (taking values from pomps)
        pParams <- list(shared=new("panelPomp")@pParams$shared,
                        specific=sapply(object,coef))
    } else {
      ## use provided params, shared or specific
      ## check for params format
      if (is.numeric(params)) params <- pParams(params)
      pParams <- params
      if (!missing(shared)) pParams$shared <- shared
      if (!missing(specific)) pParams$specific <- specific
    }
  }
  panelPomp.internal(
    pompList=if (is(object,"panelPomp")) unitobjects(object) else object,
    pParams=pParams)
}
