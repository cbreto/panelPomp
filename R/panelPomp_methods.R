## methods for panelPomp objects (other than workhorses: pfilter, mif2, etc.)

#' @include panelPomp.R
NULL

#' @name panelPomp_methods
#' @docType methods
#' @title Manipulating \code{panelPomp} objects
#' @description Tools for manipulating \code{panelPomp} objects.
#' @param object,x An object of class \code{panelPomp} or inheriting class 
#' \code{panelPomp}.
#' @param start,end position in original \code{times(pomp)} at which to start 
#' @param U how many units to keep (starting from the first).
#' and end the window.
#' @param unitname name of panel unit to be manipulated.
#' @param value value to be assigned.
#' @param ... ....
#' @section Methods:
#' \describe{
#'   \item{coef}{Extracts coefficients of \code{panelPomp} objects.}
#'   \item{coef<-}{Assign coefficients to \code{panelPomp} objects.}
#'   \item{length}{Count the number of units in \code{panelPomp} objects.}
#'   \item{names}{Get the unit names of \code{panelPomp} objects.}
#'   \item{pparams}{Extracts coefficients from the \code{pParams} slot of 
#'   \code{panelPomp} objects.}
#'   \item{unitobjects}{Extracts \code{pomp} objects from \code{panelPomp} 
#'   objects.}
#'   \item{window}{Subset \code{panelPomp} objects by changing start time, 
#'   end time, and number of units.}
#'   }
#' @author Carles Breto and Aaron A. King.
#' @family panelPomp methods
NULL

#' @rdname panelPomp_methods
#' @export
setMethod(
  "coef",
  signature=signature(object="panelPomp"),
  definition = function (object) {
    pmat <- object@pParams$specific
    c(
      object@pParams$shared,
      setNames(
        as.numeric(pmat),
        outer(rownames(pmat),colnames(pmat),sprintf,fmt="%s[%s]")
      )
    )
  }
)

#' @rdname panelPomp_methods
#' @export
setMethod(
  "coef<-",
  signature=signature(object="panelPomp"),
  definition=function (object, ..., value) {
    nn <- grep("^.+\\[.+?\\]$",names(value),perl=TRUE,value=TRUE)
    pp <- sub(pattern="^(.+?)\\[.+?\\]$",replacement="\\1",x=nn,perl=TRUE)
    uU <- names(object@unit.objects)
    pU <- sort(unique(pp))
    object@pParams$specific <- array(dim=c(length(pU),length(uU)),
                                     dimnames=list(param=pU,unit=uU))
    pvec <- setNames(numeric(length(object@pParams$specific)),
                     outer(pU,uU,sprintf,fmt="%s[%s]"))
    unitpar <- intersect(names(value),names(pvec))
    sharedpar <- setdiff(names(value),unitpar)
    pvec[unitpar] <- value[unitpar]
    object@pParams$specific[,] <- pvec
    object@pParams$shared <- value[sort(sharedpar)]
    object
  }
)

#' @rdname panelPomp_methods
#' @export
setMethod(
  "length",
  signature=signature(x="panelPomp"),
  definition = function (x) length(unitobjects(x))
)

#' @rdname panelPomp_methods
#' @export
setMethod(
  "names",
  signature=signature(x="panelPomp"),
  definition = function (x) names(x@unit.objects)
)

#' @rdname panelPomp_methods
#' @export
setMethod(
  "pparams",
  signature=signature(object="panelPomp"),
  definition = function (object) slot(object,"pParams")
)

#' @rdname panelPomp_methods
#' @export
setMethod(
  f = "unitobjects",
  signature = signature(object = "panelPomp"),
  definition = function(object,
                        unitname) {
    if (missing(unitname)) {
      return(object@unit.objects)
    } else {
      return(object@unit.objects[unitname][[1]])
    }
  }
)

#' @rdname panelPomp_methods
#' @export
setMethod(
  "window",
  signature=signature(x="panelPomp"),
  definition=function (x, U, start, end) {
    if (missing(U)) U <- length(x)
    po <- as(x,"list")[[1]]
    if (missing(start)) start <- 1
    if (missing(end)) end <- length(time(po))
    panelPomp(
      lapply(as(x,"list")[1:U],FUN=window,start=time(po)[start],
             end=time(po)[end]),
      shared=x@pParams$shared,specific=x@pParams$specific[,1:U,drop=FALSE]
    )
  }
)

## "@rdname panelPomp_methods" doesn't seem to work with setAs()
## coerce method
#' @title Coercing \code{panelPomp} objects as a \code{list}
#' @description Extracts the \code{unit.objects} slot of \code{panelPomp} 
#' objects.
#' @name as
#' @family panelPomp methods
setAs(from="panelPomp",to="list",def = function (from) from@unit.objects)
