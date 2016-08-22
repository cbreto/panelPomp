#' @include generics.R
NULL

#' An S4 class to represent panel POMP models
#'
#' @keywords internal
#' @slot unit.objects A list of \code{pomp} objects
#' @slot pParams A \code{list} of length 2 with elements named \code{shared} (a named numeric vector for 
#' parameters shared among panel units) and \code{specific} (a matrix with row parameter names and column 
#' unit names). When all parameters are specific, the numeric vector is \code{numeric(0)}. When all parameters 
#' are shared, the matrix is empty but its number of columns still represents the number of units in the panel 
#' \code{U}, i.e., \code{array(numeric(0), dim = c(0,U))}.
#'
#' @return An S4 \code{panelPomp} class object
#' 
#' @examples
#' 
#' pompExample(gompertz)
#' 
#' ppo <- panelPomp(
#'  object = list(
#'    unit1 = simulate(gompertz, seed = 12345677),
#'    unit2 = simulate(gompertz, seed = 87654321)
#'    ),
#'  shared = c(K = 1.0, r = 0.1, sigma = 0.1),
#'  specific = matrix(
#'    rep(c(0.1, 1.0), 2),
#'    ncol = 2,
#'    dimnames = list(
#'      specific.params = c("tau", "X.0"),
#'      panel.units = paste0("unit", 1:2)
#'      )
#'    )
#'  )
#'  
#'  names(ppo@pParams)
#'  dimnames(ppo@pParams$specific)
#'  class(ppo)
#'
#' @export
setClass(
  Class = "panelPomp",
  slots = c(unit.objects = "list",
            pParams = "list"),
  prototype = prototype(unit.objects = list(),
                        pParams = list(
                          shared = numeric(0),
                          specific = array(data = numeric(0),
                                           dim = c(0,0))
                          )
                        ),
  validity=function (object) {
    retval <- character(0)
    # check that mandatory arguments have been provided
    if (length(object@unit.objects)<1) {
      retval <- append(retval, paste(sQuote("unit.objects"), "is a required slot"))
    }
    # check that mandatory arguments have the required format
    if (!all(sapply(object@unit.objects,is,'pomp'))) {
      retval <- append(retval, paste0("The 'unit.objects' slot must be a list of 'pomp' objects"))
    } else {
      same.parameters.check <- T
      if (length(object) > 1) {
        for (i.u in 2:length(object)) {
          same.parameters.check <- 
            ifelse(
              all(
            sort(names(coef(object@unit.objects[[i.u]])))==sort(names(coef(object@unit.objects[[i.u - 1]])))
            ), TRUE, FALSE)
          if (same.parameters.check==FALSE) {
            retval <- append(
              retval,
              paste0(
                "error in ",sQuote("c"),
                ": `pomp' objects with different parameter names cannot be combined"
              )
            )
          }
        }
      }
    }
    # check that optional arguments have the required format
    if (!identical(list(), object@pParams)) {
      if (!is.list(object@pParams)) {
        retval <- append(retval, paste(sQuote("pParams"), "must be a list"))
      } else {
        if (!(length(object@pParams) == 2)) {
          retval <-
            append(retval, paste(sQuote("pParams"), "must be of length two"))
        } else {
          right.list.structure <-
            any(all(sapply(object@pParams, class) == c('numeric', 'matrix'))
                &
                  all(names(object@pParams) == c("shared", "specific")),
                all(sapply(object@pParams, class) == c('matrix', 'numeric'))
                &
                  all(names(object@pParams) == c("specific", "shared")))
          if (!right.list.structure) {
            retval <- append(
              retval,
              paste(
                "The elements of",
                sQuote("pParams"),
                "must be a numeric vector named 'shared' and a matrix named 'specific'"
              )
            )
          } else {
            if (!dim(object@pParams[["specific"]])[2] == length(object@unit.objects)) {
              retval <- append(
                retval,
                paste(
                  "The number of columns of the 'specific' matrix in",
                  sQuote("pParams"),
                  "must match the length of the list in the unit.object slot"
                )
              )
            } else {
              if (!identical(dimnames(object@pParams$specific)[[2]], names(object@unit.objects))) {
                retval <- append(
                  retval,
                  paste(
                    "The names of columns of the 'specific' matrix in",
                    sQuote("pParams"),
                    "must match those of the list in the unit.object slot"
                  )
                )
              } else {
                if (is.null(dimnames(object@pParams$specific)[[2]])) {
                  retval <- append(
                    retval,
                    paste(
                      "The column names of the 'specific' matrix in the",
                      sQuote("pParams"),
                      "slot must be non-empty"
                    )
                  )
                } else {
                  if (is.null(names(object@unit.objects))) {
                    retval <- append(
                      retval,
                      paste(
                        "The names of the 'list' in the",
                        sQuote("unit.object"),
                        "slot must be non-empty"
                      )
                    )
                  } else {
                pParams.names <-
                  c(names(object@pParams$shared),
                    dimnames(object@pParams$specific)[[1]])
                if (!identical(sort(pParams.names), sort(names(coef(
                  object@unit.objects[[1]]
                ))))) {
                  retval <- append(
                    retval,
                    paste(
                      "All parameters in the pomp objects of unit.objects slot must be in",
                      sQuote("pParams"),
                      "and viceversa"
                    )
                  )
                }
                  }
                }
              }
            }
          }
        }
      }
    }
    if (length(retval) == 0)
      TRUE
    else
      retval
  }
)
