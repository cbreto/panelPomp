## panelPomp: class and constructor

#' @include panel_logmeanexp.R
NULL

## panelPomp class

#' @keywords internal
#' @export
setClass(
  "panelPomp",
  slots=c(
    unit.objects = "list",
    pParams = "list"
  ),
  prototype=prototype(
    unit.objects=list(),
    pParams=list(shared=numeric(0),specific=array(numeric(0),dim=c(0,0)))
  ),
  validity=function (object) {
    retval <- character(0)
    # check that mandatory arguments have the required format
    if (!all(sapply(object@unit.objects,is,'pomp'))) {
      retval <- append(retval, paste0("The 'unit.objects' slot must be a list of 'pomp' objects"))
    } else {
      same.parameters.check <- TRUE
      if (length(object) > 1) {
        for (i.u in 2:length(object)) {
          same.parameters.check <- 
            ifelse(
              all(
                sort(names(coef(object@unit.objects[[i.u]])))==sort(names(coef(object@unit.objects[[i.u - 1]])))
              ), TRUE, FALSE)
          if (same.parameters.check==FALSE) {
            retval <- append(
              retval,
              paste0(
                "error in ",sQuote("c"),
                ": `pomp' objects with different parameter names cannot be combined"
              )
            )
          }
        }
      }
    }
    # check that optional arguments have the required format
    if (!identical(list(), object@pParams)) {
      if (!is.list(object@pParams)) {
        retval <- append(retval, paste(sQuote("pParams"), "must be a list"))
      } else {
        if (!(length(object@pParams) == 2)) {
          retval <-
            append(retval, paste(sQuote("pParams"), "must be of length two"))
        } else {
          right.list.structure <-
            any(all(sapply(object@pParams, class) == c('numeric', 'matrix'))
                &
                  all(names(object@pParams) == c("shared", "specific")),
                all(sapply(object@pParams, class) == c('matrix', 'numeric'))
                &
                  all(names(object@pParams) == c("specific", "shared")))
          if (!right.list.structure) {
            retval <- append(
              retval,
              paste(
                "The elements of",
                sQuote("pParams"),
                "must be a numeric vector named 'shared' and a matrix named 'specific'"
              )
            )
          } else {
            if (!dim(object@pParams[["specific"]])[2] == length(object@unit.objects)) {
              retval <- append(
                retval,
                paste(
                  "The number of columns of the 'specific' matrix in",
                  sQuote("pParams"),
                  "must match the length of the list in the unit.object slot"
                )
              )
            } else {
              if (!identical(dimnames(object@pParams$specific)[[2]], names(object@unit.objects))) {
                retval <- append(
                  retval,
                  paste(
                    "The names of columns of the 'specific' matrix in",
                    sQuote("pParams"),
                    "must match those of the list in the unit.object slot"
                  )
                )
              } else {
                if (is.null(dimnames(object@pParams$specific)[[2]])) {
                  retval <- append(
                    retval,
                    paste(
                      "The column names of the 'specific' matrix in the",
                      sQuote("pParams"),
                      "slot must be non-empty"
                    )
                  )
                } else {
                  if (is.null(names(object@unit.objects))) {
                    retval <- append(
                      retval,
                      paste(
                        "The names of the 'list' in the",
                        sQuote("unit.object"),
                        "slot must be non-empty"
                      )
                    )
                  } else {
                    pParams.names <-c(
                      names(object@pParams$shared),
                      dimnames(object@pParams$specific)[[1]]
                    )
                    if (!identical(
                      sort(pParams.names),
                      sort(names(coef(object@unit.objects[[1]]))))) retval <- append(
                        retval,
                        paste(
                          "All parameters in the pomp objects of unit.objects slot must be in",
                          sQuote("pParams"),
                          "and viceversa"
                        )
                      )
                  }
                }
              }
            }
          }
        }
      }
    }
    if (length(retval)==0) TRUE else retval
  }
)

## panelPomp constructor

pPomp.internal <- function(pompList,pParams,
                           verbose=getOption("verbose",FALSE)) {
  # If needed, fix validity checks on 'pParams$specific'
  if (identical(pParams$specific,array(numeric(0),dim=c(0,0)))) {
    pParams$specific <- array(
      numeric(0),
      dim=c(0,length(pompList)),
      dimnames=list(params=character(0),unit=names(pompList))
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
    parnames <- c(names(object@pParams$shared),row.names(object@pParams$sp))
    stopifnot(all(shared%in%parnames))
    sp <- parnames[!parnames%in%shared]
    # make matrix from object@pParams$sh that can be rbinded to object@pParams$sp
    sh0 <- names(object@pParams$shared)
    not.in.sp0 <- matrix(
      object@pParams$shared,nrow=length(sh0),ncol=length(object),
      dimnames=list(sh0,names(object))
    )
    all.sp <- rbind(object@pParams$specific,not.in.sp0)
    stopifnot(!as.logical(anyDuplicated(row.names(all.sp))))
    # make vector from object@pParams$sp[1,] that can be c()d to object@pParams$sh
    all.sh <- c(object@pParams$shared,object@pParams$specific[,1])
    
    shs <- all.sh[shared]
    sps <- all.sp[sp,]
    panelPomp(as(object,"list"),shared=shs,specific=sps)
  }
)
