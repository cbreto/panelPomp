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
#' @param params optional; a named numeric vector. In this case, the nature of 
#' parameters is determined via a naming convention: names ending in 
#' \dQuote{\code{[unit_name]}} are assumed to denote unit-specific parameters;
#' all other names specify shared parameters.
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
    shared = 'numeric',
    specific = 'matrix'
  ),
  prototype=prototype(
    unit.objects=list(),
    shared=numeric(),
    specific=matrix(numeric(),0,0)
  ),
  validity=function (object) {
    
    retval <- character(0)
    
    ## check to make sure unit.objects is named and are all pomps
    if (length(object@unit.objects)<1) {
      retval <- append(
        retval,wQuotes("a ''panelPomp'' must contain at least one ''pomp''"))
    } else {
      u.names <- names(object@unit.objects)
      if (is.null(u.names)) {
        retval <- append(retval,wQuotes("''unit.object'' must have names"))
      }
      if (!all(sapply(object@unit.objects,is,"pomp"))) {
        retval <- append(retval,
          wQuotes("''unit.objects'' must be a list of ''pomp'' objects"))
      }
    }

    sh.names <- names(object@shared)
    sp.names <- rownames(object@specific)
    
    if ((is.null(sh.names) && length(object@shared)>0) ||
        (is.null(sp.names) && length(object@specific)>0))
      retval <- append(retval,
        wQuotes("all parameters must be named"))
    
    if (length(intersect(sh.names,sp.names))>0)
      retval <- append(retval,"a parameter cannot be both shared and specific!")
    
    if (!setequal(u.names,colnames(object@specific)))
      retval <- append(retval,
        wQuotes("the column names of the specific parameter matrix must",
          " match the names of the units"))
    
    if (length(retval)==0) 
      TRUE 
    else {
      append(retval," (validity check)")
      retval
    }
  }
)


#' @rdname panelPomp
#' @export
panelPomp <- function (object, shared, specific, params) {
  
  ep <- wQuotes("in ''panelPomp'': ")
  
  if (missing(object))
    stop(wQuotes(ep,"''object'' is a required argument."),call.=FALSE)
  
  sh.given <- !missing(shared)
  sp.given <- !missing(specific)
  pv.given <- !missing(params)
  
  if (is.list(object) && all(sapply(object,is,"pomp"))) {
    ## object should be a list of pomps
    ## construct a panelPomp
    ## if no parameters provided, ...
    if (is.null(names(object))) {
      nnm <- ceiling(log10(length(object)+1))
      names(object) <- sprintf(sprintf("unit%%0%dd",nnm),seq_len(length(object)))
    }
    if (any(names(object)=="")) {
      stop(ep,"empty unit names are not permitted",call.=FALSE)
    }
    pp <- lapply(object,coef)
    upn <- unique(do.call(c,lapply(pp,names)))
    specp <- array(
      dim=c(length(upn),length(object)),
      dimnames=list(parameter=upn,unit=names(object))
    )
    for (u in seq_along(object)) {
      specp[names(pp[[u]]),u] <- pp[[u]]
      coef(object[[u]]) <- numeric(0)
    }
    if (any(is.na(specp))) {
      warning(ep,"NAs in specific parameters",call.=FALSE)
    }
    object <- new(
      "panelPomp",
      unit.objects=object,
      shared=new("panelPomp")@shared,
      specific=specp
    )
  } else if (!is(object,"panelPomp")) {
    stop(wQuotes(ep,"''object'' must be either a ''panelPomp'' object or a",
      " list of ''pomp'' objects."),
      call.=FALSE)
  }
  
  if (pv.given) {  ## parameters are specified using vector 'params'
    if (sh.given || sp.given) {
      stop(wQuotes(ep,
        "specify EITHER ''params'' OR ''shared'' and/or ''specific''."),
        call.=FALSE)
    } else {
      if (is.numeric(params) && !is.null(names(params))) {
        params <- pParams(params)
      } else {
        stop(wQuotes(ep,"''params'' must be a named numeric vector"),call.=FALSE)
      }
      if (length(params$specific)==0) {
        params$specific <- array(
          dim=c(0,length(object@unit.objects)),
          dimnames=list(parameter=character(0),unit=names(object@unit.objects))
          )
      }
      object <- new("panelPomp",unit.objects=unitobjects(object),
        shared=params$shared,specific=params$specific)
    }
  } else {  ## we are changing the allocation between shared and specific
    
    u.names <- names(object@unit.objects)
    osp.names <- rownames(object@specific)
    osh.names <- names(object@shared)
    
    if (sh.given) {  ## get names of parameters that are to be shared
      if (is.null(shared)) {
        shared <- numeric(0)
        sh.names <- NULL
      } else if (is.numeric(shared) && !is.null(names(shared))) {
        sh.names <- names(shared)
      } else {
        stop(wQuotes(ep,"''shared'' must be a named numeric vector or NULL"),
          call.=FALSE)
      }
    }
    
    if (sp.given) { ## get names of parameters that are to be specific
      if (is.character(specific)) {
        sp.names <- unname(specific)
      } else if (is.numeric(specific) && !is.matrix(specific)) {
        sp.names <- names(specific)
        if (is.null(sp.names) || any(sp.names==""))
          stop(wQuotes(ep,"if given as a vector, ''specific'' must have names"),
            call.=FALSE)
        specific <- array(
          data=specific,
          dim=c(length(sp.names),length(u.names)),
          dimnames=list(parameter=sp.names,unit=u.names)
        )
      } else if (is.numeric(specific) && is.matrix(specific)) {
        sp.names <- rownames(specific)
        if (!setequal(colnames(specific),u.names))
          stop(wQuotes(ep,
            "column names of ''specific'' must correspond to names of units"),
            call.=FALSE)
      } else {
        stop(wQuotes(ep,"''specific'' must be furnished as a numeric matrix, ",
          "a numeric vector, or a character vector"),call.=FALSE)
      }
    }
    
    if (sp.given) {
      if (is.character(specific)) {
        if (sh.given) {
          if (length(intersect(sh.names,sp.names))>0)
            stop(wQuotes(ep,"a parameter cannot be both shared and specific!"),
              call.=FALSE)
          if (!all(sp.names %in% c(osh.names,osp.names))) {
            msps <- sp.names[!(sp.names %in% c(osh.names,osp.names))]
            stop(ep,"the following parameters are to be treated as specific, ",
              "but no values for them are specified: ",
              paste(sQuote(msps),collapse=","),call.=FALSE)
          }
          osps <- intersect(osp.names,sp.names)
          nsps <- intersect(osh.names,sp.names)
          object <- new(
            "panelPomp",
            unit.objects=unitobjects(object),
            shared=shared,
            specific=rbind(
              object@specific[osps,,drop=FALSE],
              array(
                data=object@shared[nsps],
                dim=c(length(nsps),length(u.names)),
                dimnames=list(parameter=nsps,unit=u.names)
              )
            )
          )
        } else {
          if (!all(sp.names %in% c(osp.names,osh.names))) {
            msps <- sp.names[!(sp.names %in% c(osp.names,osh.names))]
            stop(ep,"the following parameters are to be treated as specific, ",
              "but no values for them are specified: ",
              paste(sQuote(msps),collapse=","),call.=FALSE)
          }
          osps <- intersect(osp.names,sp.names)
          nsps <- intersect(osh.names,sp.names)
          nshs <- setdiff(osh.names,sp.names)
          object <- new(
           "panelPomp",
            unit.objects=unitobjects(object),
            shared=object@shared[nshs,drop=FALSE],
            specific=rbind(
              object@specific[osps,,drop=FALSE],
              array(
                data=object@shared[nsps],
                dim=c(length(nsps),length(u.names)),
                dimnames=list(parameter=nsps,unit=u.names)
              )
            )
          )
       }
      } else { ## 'specific' is a matrix
        if (sh.given) {
          object <- new(
            "panelPomp",
            unit.objects=unitobjects(object),
            shared=shared,
            specific=specific
          )
        } else {
          nshs <- setdiff(osh.names,sp.names)
          object <- new(
            "panelPomp",
            unit.objects=unitobjects(object),
            shared=object@shared[nshs],
            specific=specific
          )
        }
      }
    } else {
      if (sh.given) {
        nsps <- setdiff(osp.names,sh.names)
        object <- new(
          "panelPomp",
          unit.objects=unitobjects(object),
          shared=shared,
          specific=object@specific[nsps,,drop=FALSE]
        )
      }
    }
  }
  
  object
}
