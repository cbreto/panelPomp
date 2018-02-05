#' @include panelPomp-class.R
NULL

## basic constructor of the panelPomp class
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
