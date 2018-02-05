#' @include panelPomp-construction.R
NULL

#' An S4 class to represent panel POMP models that have been analyzed using \code{mif2}
#'
#' @keywords internal
#' @inheritParams panelPomp
#' @slot Np Np
#' @slot ploglik panel loglikelihoods
#' @slot tol panel tolerances
#' @slot unit.logliks unit loglikelihoods#' 
#'
#' @export
setClass(
  Class = "pfilterd.ppomp",
  contains = "panelPomp",
  slots = c(
    Np = "numeric",
    ploglik = "numeric",
    tol = "numeric",
    unit.logliks = "numeric"
  ),
  prototype = prototype(
    Np = as.integer(NA),
    ploglik = as.double(NA),
    tol = as.double(NA),
    unit.logliks = numeric(0)
  )
)
