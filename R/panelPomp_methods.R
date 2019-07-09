## methods for panelPomp objects (other than workhorses: pfilter, mif2, etc.)

#' @include panelPomp.R
NULL

#' @name panelPomp_methods
#' @docType methods
#' @title Manipulating \code{panelPomp} objects
#' @description Tools for manipulating \code{panelPomp} objects.
#' @param object,x An object of class \code{panelPomp} or inheriting class 
#' \code{panelPomp}.
#' @param start,end position in original \code{times(pomp)} at which to start.
#' @param i unit index (indices) or name (names).
#' @param value value to be assigned.
#' @param ... ....
#' @section Methods:
#' \describe{
#'   \item{coef}{Extracts coefficients of \code{panelPomp} objects.}
#'   \item{coef<-}{Assign coefficients to \code{panelPomp} objects.}
#'   \item{length}{Count the number of units in \code{panelPomp} objects.}
#'   \item{names}{Get the unit names of \code{panelPomp} objects.}
#'   \item{pparams}{Extracts coefficients from \code{panelPomp} objects.}
#'   \item{[]}{Take a subset of units.}
#'   \item{[[]]}{Select the pomp object for a single unit.}
#'   \item{window}{Subset \code{panelPomp} objects by changing start time and
#'   end time.}
#'   }
#' @author Carles Breto, Aaron A. King.
#' @family panelPomp methods
NULL

#' @rdname panelPomp_methods
#' @export
setMethod(
  "coef",
  signature=signature(object="panelPomp"),
  definition = function (object) {
    pmat <- object@specific
    c(
      object@shared,
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
    ## check names(value)
    ep <- paste0("in ",sQuote("coef<-"),": ")
    if (is.list(value)) value <- unlist(value)
    if (!identical(character(0),setdiff(names(value),names(coef(object))))) 
      stop(paste0(ep,"part of ",sQuote("value")," is not part of ",
                  sQuote("coef(object)"),"."),call.=FALSE)
    if (!identical(character(0),setdiff(names(coef(object)),names(value)))) 
      stop(paste0(ep,"part of ",sQuote("coef(object)")," is not specified in ",
                  sQuote("value"),"."),call.=FALSE)
    nn <- grep("^.+\\[.+?\\]$",names(value),perl=TRUE,value=TRUE)
    pp <- sub(pattern="^(.+?)\\[.+?\\]$",replacement="\\1",x=nn,perl=TRUE)
    uU <- names(object@unit.objects)
    pU <- sort(unique(pp))
    object@specific <- array(dim=c(length(pU),length(uU)),
                                     dimnames=list(param=pU,unit=uU))
    pvec <- setNames(numeric(length(object@specific)),
                     outer(pU,uU,sprintf,fmt="%s[%s]"))
    unitpar <- intersect(names(value),names(pvec))
    sharedpar <- setdiff(names(value),unitpar)
    pvec[unitpar] <- value[unitpar]
    object@specific[,] <- pvec
    object@shared <- value[sort(sharedpar)]
    object
  }
)

#' @rdname panelPomp_methods
#' @export
setMethod(
  "length",
  signature=signature(x="panelPomp"),
  definition = function (x) length(x@unit.objects)
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
  definition = function (object) 
    list(shared=object@shared,specific=object@specific)
)

#' @rdname panelPomp_methods
#' @export
pParams <- function (value) {
  nn <- grep("^.+\\[.+?\\]$",names(value),perl=TRUE,value=TRUE)
  shs <- names(value)[!names(value)%in%nn]
  pp <- sub(pattern="^(.+?)\\[.+?\\]$",replacement="\\1",x=nn,perl=TRUE)
  sps <- sort(unique(pp))
  uu <- sub(pattern="^(.+?)\\[(.+?)\\]$",replacement="\\2",x=nn,perl=TRUE)
  us <- sort(unique(uu))
  pParams <- list(shared=numeric(0),specific=array(numeric(0),dim=c(0,0)))
  if (length(shs)>0) pParams$shared <- value[shs]
  if (length(sps)>0) {
    pParams$specific <- array(dim=c(length(sps),length(us)),
                              dimnames=list(sps,us))
    for (sp in sps) {
      pParams$specific[sp,us] <- value[paste0(sp,"[",us,"]")]
    }
  }
  pParams
}

#' @rdname panelPomp_methods
#' @export
setMethod(
  "print",
  signature=signature(x="panelPomp"),
  definition=function (x, ...) {
    cat("<object of class ",sQuote("panelPomp"),">\n",sep="")
    invisible(x)
  }
)

#' @rdname panelPomp_methods
#' @export
setMethod(
  "show",
  signature=signature(object="panelPomp"),
  definition=function (object) {
    print(object)
    cat("panel of",length(object),ifelse(length(object)>1,"units","unit"),"\n")
    if (length(coef(object))>0) {
      cat("parameter(s):\n")
      print(pParams(coef(object)))
    } else {
      cat("parameter(s) unspecified\n");
    }
    cat(paste0("summary of first panel unit (\"",names(object)[1],"\"):","\n"))
    show(object[[1]])
    invisible(NULL)
  }
)

#' @rdname panelPomp_methods
#' @export
setMethod(
  "unitobjects",
  signature = signature(object = "panelPomp"),
  definition = function(object) {
    object@unit.objects
  }
)

#' @rdname panelPomp_methods
#' @export
setMethod(
  "window",
  signature=signature(x="panelPomp"),
  definition=function (x, start, end) {
    tm <- time(x[[1]],t0=FALSE)
    if (missing(start)) start <- tm[1]
    if (missing(end)) end <- tm[length(tm)]
    panelPomp(
      lapply(x@unit.objects,FUN=window,start=start,end=end),
      shared=x@shared,
      specific=x@specific
    )
  }
)

#' @rdname panelPomp_methods
#' @export
setMethod(
  "[",
  signature=signature(x="panelPomp"),
  definition=function (x, i) {
    panelPomp(
      x@unit.objects[i],
      shared=x@shared,
      specific=x@specific[,i,drop=FALSE]
    )
  }
)

#' @rdname panelPomp_methods
#' @export
setMethod(
  "[[",
  signature=signature(x="panelPomp"),
  definition=function (x, i) {
    po <- x@unit.objects[[i]]
    coef(po) <- c(x@shared,setNames(x@specific[,i],rownames(x@specific)))
    po
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

## coerce method
#' @title Coercing \code{panelPomp} objects as a \code{data.frame}
#' @description Coerces a \code{panelPomp} into a data frame.
#' @name as
#' @family panelPomp methods
setAs(from="panelPomp",to="data.frame",
      def = function (from) {
        x <- lapply(from@unit.objects,as,"data.frame")
        for (u in seq_along(x)) {
          x[[u]]$unit <- names(from@unit.objects)[[u]]
        }
        do.call(rbind,x)
      }
)

