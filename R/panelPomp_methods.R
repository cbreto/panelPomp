## methods for panelPomp objects (other than workhorses: pfilter, mif2, etc.)

#' @include panelPomp.R
NULL

## 'coef' method for panelPomp signature
#' Extract coefficients from the \code{pParams} slot of \code{panelPomp} objects.
#'
#' Extract coefficients from the \code{pParams} slot of \code{panelPomp} objects.
#'
#' Extract coefficients from the \code{pParams} slot of \code{panelPomp} objects.
#'
#' @param object A \code{panelPomp} object.
#'
#' @export
#'
setMethod(
  "coef",
  signature=signature(object="panelPomp"),
  definition = function (object) object@pParams
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
  definition = function (x) names(as(x,"list"))
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
              shared = coef(x)$shared,
              specific = coef(x)$specific[, 1:U]
    )
  }
)
