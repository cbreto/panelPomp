## panelPomp: class and constructor

#' @include panel_logmeanexp.R
NULL

#' @title Constructing \code{panelPomp} objects
#' @name panelPomp
#' @description This function constructs a \code{panelPomp} object.
#' @param object required; either (i) a named \code{list} of \code{pomp} 
#' objects; or (ii) an object of class \code{panelPomp} or inheriting class 
#' \code{panelPomp}. In the second case, the function \code{panelPomp} allows 
#' modifying the provided object.
#' @param shared optional; either (matching the \code{object} argument above) 
#' (i) a named \code{numeric vector}; or (ii) a character vector of names for 
#' parameters to be set to shared. Parameters that were not originally shared 
#' are copied from the specific parameters from the first panel unit (in the 
#' second case, it defaults to \code{NULL}, which implies all parameters are 
#' specific).
#' @param specific optional; a \code{matrix} with parameters on named rows and panel 
#' units on named columns.
#' @param params optional; a list with (named) 'shared' and 'specific' elements.
#' @references \breto2017
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
