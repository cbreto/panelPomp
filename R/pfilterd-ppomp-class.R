#' @include panelPomp-methods-for-construction.R
NULL

#' An S4 class to represent panel POMP models that have been analyzed using \code{mif2}
#'
#' @keywords internal
#' @inheritParams panelPomp
#' @slot Np Np
#' @slot ploglik panel loglikelihoods
#' @slot ptol panel tolerances
#' @slot unit.logliks unit loglikelihoods#' 
#'
#' @export
setClass(
  Class = "pfilterd.ppomp",
  contains = "panelPomp",
  slots = c(
    Np = "numeric",
    ploglik = "numeric",
    ptol = "numeric",
    unit.logliks = "numeric"
  ),
  prototype = prototype(
    Np = numeric(0),
    ploglik = as.double(NA),
    ptol = numeric(0),
    unit.logliks = numeric(0)
  )
)