#' @include generics.R
NULL

## define the panelPomp class

#' @keywords internal
#' @export
setClass(
  "panelPomp",
  slots=c(
    unit.objects = "list",
    pParams = "list"
  ),
  prototype=prototype(
    unit.objects=list(),
    pParams=list(shared=numeric(0),specific=array(numeric(0),dim=c(0,0)))
  ),
  validity=function (object) {
    retval <- character(0)
    # check that mandatory arguments have the required format
    if (!all(sapply(object@unit.objects,is,'pomp'))) {
      retval <- append(retval, paste0("The 'unit.objects' slot must be a list of 'pomp' objects"))
    } else {
      same.parameters.check <- TRUE
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
                    pParams.names <-c(
                      names(object@pParams$shared),
                      dimnames(object@pParams$specific)[[1]]
                    )
                    if (!identical(
                      sort(pParams.names),
                      sort(names(coef(object@unit.objects[[1]]))))) retval <- append(
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
    if (length(retval)==0) TRUE else retval
  }
)
