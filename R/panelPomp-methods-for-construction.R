#' @include panelPomp-methods.R
NULL

#' Construct \code{panelPomp} objects.
#'
#' S4 method.
#'
#' S4 method.
#'
#' @param object A \code{list} of \code{pomp} objects.
#' @param shared A named \code{numeric vector}
#' @param specific A \code{matrix} with parameters on named rows and panel units on named columns  
#' more than one column
#'
#' @export
#'
setMethod(
  f = "panelPomp",
  signature = signature(object = "list"),
  definition = function(object,
                        shared = numeric(0),
                        specific = array(data = numeric(0), dim = c(0, 0))) {
    if (missing(object))
      stop(sQuote("object"),
           " (list of pomp objects) is a required argument")
    # If no shared nor specific parameters are provided, endow the panelPomp object with an empty pParams slot 
    if (
      identical(x = shared, y = numeric(0)) && 
      identical(x = specific, y = array(data = numeric(0), dim = c(0, 0)))
    ) {
      pParams <- list()
    } else {
    # Else, used the parameters specified by the user
      pParams <- list(shared = shared, specific = specific)
    }
    pPomp.internal(pompList = object,
                   pParams = pParams)
  }
)
# END setMethod