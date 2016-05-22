#' @include panelPomp-class.R
NULL

## basic constructor of the panelPomp class
pPomp.internal <- function(pompList,
                           pParams,
                           verbose = getOption("verbose",FALSE)) {
  new(Class = "panelPomp",
      unit.objects = pompList,
      pParams = pParams)
}# END FN panelPomp.constructor
