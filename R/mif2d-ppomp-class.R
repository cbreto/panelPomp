#' @include pfilterd-ppomp-methods.R
NULL

#' An S4 class to represent panel POMP models that have been analyzed using \code{mif2}
#'
#' @keywords internal
#' @inheritParams pfilterd.ppomp
#' @slot Nmif Nmif
#' @slot prw.sd This slot is a list of unit-specific rw.sd's (the argument of pomp::mif2)
#' @slot cooling.type cooling.type
#' @slot cooling.fraction.50 cooling.fraction.50
#' @slot transform transform
#' @slot pconv.rec pconv.rec
#' @slot pconv.rec.array pconv.rec.array
#'
#' @export
setClass(
  Class = "mif2d.ppomp",
  contains = "pfilterd.ppomp",
  slots = c(
    Nmif = "integer",
    prw.sd = "list",
    cooling.type = "character",
    cooling.fraction.50 = "numeric",
    transform = "logical",
    pconv.rec = "matrix",
    pconv.rec.array = "array"),
  prototype = prototype(
    Nmif = integer(0),
    prw.sd = list(),
    cooling.type = character(0),
    cooling.fraction.50 = numeric(0),
    transform = F,
    pconv.rec = array(data = numeric(0), dim = c(0, 0)),
    pconv.rec.array = array(data = numeric(0), dim = c(0, 0, 0))
    )
)