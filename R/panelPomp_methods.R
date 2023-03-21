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
#'   \item{pparams}{Extracts coefficients from \code{panelPomp} objects in list form.}
#'   \item{pParams}{Converts panel coefficients from vector form to list form.}
#'   \item{window}{Subset \code{panelPomp} objects by changing start time and
#'   end time.}
#'   \item{[]}{Take a subset of units.}
#'   \item{[[]]}{Select the pomp object for a single unit.}
#'   }
#' @author Carles \Breto, Aaron A. King.
#' @family panelPomp methods
NULL

#' @rdname panelPomp_methods
#' @return
#' \code{coef()} returns a \code{numeric} vector.
#' @examples
#' ## access and manipulate model parameters and other features
#' prw <- panelRandomWalk()
#' coef(prw)
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
#' @examples
#' # replace coefficients
#' coef(prw) <- c(sigmaX=2,coef(prw)[-1])
#' coef(prw)
#' @export
setMethod(
  "coef<-",
  signature=signature(object="panelPomp"),
  definition=function (object, ..., value) {
    ## check names(value)
    ep <- wQuotes("in ''coef<-'': ")
    if (is.list(value)) value <- unlist(value)
    if (!identical(character(0),setdiff(names(value),names(coef(object)))))
      stop(wQuotes(ep,"part of ''value'' is not part of ''coef(object)''","."),call.=FALSE)
    if (!identical(character(0),setdiff(names(coef(object)),names(value))))
    stop(wQuotes(ep,"part of ''coef(object)'' is not specified in ''value''","."),
         call.=FALSE)
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
#' @return
#' \code{length()} returns an \code{integer}.
#' @examples
#' length(prw)
#' @export
setMethod(
  "length",
  signature=signature(x="panelPomp"),
  definition = function (x) length(x@unit.objects)
)

#' @rdname panelPomp_methods
#' @return
#' \code{names()} returns a \code{character} vector.
#' @examples
#' names(prw)
#' @export
setMethod(
  "names",
  signature=signature(x="panelPomp"),
  definition = function (x) names(x@unit.objects)
)

#' @rdname panelPomp_methods
#' @return
#' \code{pparams()} returns a \code{matrix} with the model parameters in \code{list} form.
#' @examples
#' # extract parameters in list form
#' pparams(prw)
#' @export
setMethod(
  "pparams",
  signature=signature(object="panelPomp"),
  definition = function (object)
    list(shared=object@shared,specific=object@specific)
)

#' @rdname panelPomp_methods
#' @return
#' \code{pParams()} returns a \code{list} with the model parameters in list form.
#' @examples
#' # convert vector-form parameters to list-form parameters
#' pParams(coef(prw))
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
                              dimnames=list(param=sps,unit=us))
    for (sp in sps) {
      pParams$specific[sp,us] <- value[paste0(sp,"[",us,"]")]
    }
  }
  pParams
}

#' @rdname panelPomp_methods
#' @export
#' @examples
#' ## summaries of objects
#' print(prw)
#' @export
setMethod(
  "print",
  signature=signature(x="panelPomp"),
  definition=function (x, ...) {
    cat(wQuotes("<object of class ''panelPomp''>\n"))
    invisible(x)
  }
)

#' @rdname panelPomp_methods
#' @examples
#' show(prw)
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
#' @return
#' \code{unitobjects()} returns a \code{list} of \code{pomp} objects.
#' @examples
#' ## access underlying pomp objects
#' unitobjects(prw)
#' @export
setMethod(
  "unitobjects",
  signature = signature(object = "panelPomp"),
  definition = function(object) {
    object@unit.objects
  }
)

#' @rdname panelPomp_methods
#' @return
#' \code{window()} returns a \code{panelPomp} object with adjusted times.
#' @examples
#' ## select windows of time
#' window(prw,start=2,end=4)
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
#' @return
#' \code{`[`} returns a \code{panelPomp} object.
#' @examples
#' ## subsetting panelPomp objects
#' prw[1] # panelPomp of 1 unit (first unit of prw)
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
#' @return
#' \code{`[[`} returns a \code{pomp} object.
#' @examples
#' prw[[2]] # pomp object corresponding to unit 2 of prw
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
#' @title Coercing \code{panelPomp} objects as \code{list}, \code{pompList} or
#' \code{data.frame}
#' @description When coercing to a \code{list}, it extracts the
#' \code{unit.objects} slot of \code{panelPomp} objects and attaches
#' associated parameters.
#' @name as
#' @family panelPomp methods
#' @author Carles \Breto
#' @return
#' An object of class matching that specified in the second argument (\code{to=}).
#' @examples
#' prw <- panelRandomWalk()
#' as(prw,'list')
setAs(from="panelPomp",to="list",def = function (from) {
  plist <- from@unit.objects
  shared <- from@shared
  specific <- from@specific
  for(u in 1:length(plist)) {
    coef(plist[[u]]) <- c(shared,setNames(specific[,u],rownames(specific)))
  }
  plist
})

# [seems to be overwritten by first call] @title Coercing \code{panelPomp} objects as a \code{pompList}
#' @description When coercing to a \code{pompList}, it extracts the
#' \code{unit.objects} slot of \code{panelPomp} objects and attaches
#' associated parameters, converting the resulting list to a \code{pompList} to
#' help the assignment of pomp methods.
#' @name as
# [seems to simply replicate 'see also'] @family panelPomp methods
# @author Carles \Breto
#' @examples
#' as(prw,'pompList')
setAs(from="panelPomp",to="pompList",def = function (from) {
  plist <- as(from,"list")
  class(plist) <- "pompList"
  plist
})

## coerce method
# [seems to be overwritten by first call] @title Coercing \code{panelPomp} objects as a \code{data.frame}
#' @description When coercing to a \code{data.frame}, it coerces a
#' \code{panelPomp} into a \code{data.frame}, assuming units share common
#' variable names.
#' @name as
# [seems to simply replicate 'see also'] @family panelPomp methods
# @author Carles \Breto
#' @examples
#' as(prw,'data.frame')
setAs(from="panelPomp",to="data.frame",
      def = function (from) {
        x <- lapply(from@unit.objects,as,"data.frame")
        for (u in seq_along(x)) {
          x[[u]]$unit <- names(from@unit.objects)[[u]]
        }
        do.call(rbind,x)
      }
)
