## methods for panelPomp objects (other than workhorses: pfilter, mif2, etc.)

#' @include panelPomp.R
NULL

## 'coef' method for panelPomp signature
#' @title Extract coefficients from the \code{pParams} slot of \code{panelPomp} objects.
#' @description Extract coefficients from the \code{pParams} slot of \code{panelPomp} objects.
#' @details Extract coefficients from the \code{pParams} slot of \code{panelPomp} objects.
#' @param object A \code{panelPomp} object.
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

## 'coef<-' method for panelPomp signature
#' @title Extract coefficients from the \code{pParams} slot of \code{panelPomp} objects.
#' @description Extract coefficients from the \code{pParams} slot of \code{panelPomp} objects.
#' @details Extract coefficients from the \code{pParams} slot of \code{panelPomp} objects.
#' @param object A \code{panelPomp} object.
#' @param value value.
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

## 'coerce' method: allows for coercion of a "panelPomp" object to a list
#' Extract \code{unit.objects} slot of \code{panelPomp} objects as a \code{list}.
#'
#' Extract \code{unit.objects} slot of \code{panelPomp} objects as a \code{list}.
#'
#' Extract \code{unit.objects} slot of \code{panelPomp} objects as a \code{list}.
#'
#' @name as
#' @family panelPomp
#'
setAs(from="panelPomp",to="list",def = function (from) from@unit.objects)


### length method for panelPomp signature
#' Count the number of units in the \code{unitobjects} slot of \code{panelPomp} objects.
#'
#' Count the number of units in the \code{unitobjects} slot of \code{panelPomp} objects.
#'
#' Count the number of units in the \code{unitobjects} slot of \code{panelPomp} objects.
#'
# Can't @inheritParams coef because coef's argument is "object" not "x" (which is length's argument)
#' @param x A \code{panelPomp} object.
#'
#' @export
#'
setMethod(
  "length",
  signature=signature(x="panelPomp"),
  definition = function (x) length(unitobjects(x))
)

### names method for panelPomp signature
#' Get the unit names of a \code{panelPomp} objects.
#'
#' Get the unit names of a \code{panelPomp} objects.
#'
#' S4 method.
#'
# Can't @inheritParams coef because coef's argument is "object" not "x" (which is length's argument)
#' @param x A \code{panelPomp} object.
#'
#' @export
#'
setMethod(
  "names",
  signature=signature(x="panelPomp"),
  definition = function (x) names(x@unit.objects)
)

### unitobjects method for panelPomp signature
#' Extract individual \code{pomp} objects from \code{panelPomp} objects.
#'
#' S4 method
#'
#' S4 method.
#'
#' @param object A \code{panelPomp} object.
#' @param unitname unitname.
#'
#' @export
#'
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
)# END setMethod



#' Subset panelPomp objects by changing start time, end time, and number of units.
#'
#' Subset panelPomp objects by changing start time, end time, and number of units.
#'
#' @param x panelPomp object.
#' @param U how many units to keep (starting from the first).
#' @param start position in original \code{times(pomp)} at which to start the window.
#' @param end position in original \code{time(pomp)} at which to end the window.
#'
#' @export
#'
setMethod(
  f = "window",
  signature = signature(x = "panelPomp"),
  definition = function (x, U, start, end) {
    panelPomp(lapply(as(x, "list")[1:U],
                     FUN = window,
                     start = time(as(x, "list")[[1]])[start],
                     end = time(as(x, "list")[[1]])[end]),
              shared = x@pParams$shared,
              specific = x@pParams$specific[, 1:U]
    )
  }
)
