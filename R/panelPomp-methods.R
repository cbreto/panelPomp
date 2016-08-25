#' @include panelPomp-internal.R
NULL

### coef method for panelPomp signature
#' Extract coefficients from the \code{pParams} slot of \code{panelPomp} objects.
#'
#' What do I do?
#'
#' S4 method.
#'
#' @param object A \code{panelPomp} object.
#'
#' @export
#'
setMethod(
  f = "coef",
  signature = signature(object = "panelPomp"),
  definition = function(object) {object@pParams}
)# END setMethod



### length method for panelPomp signature
#' Count the number of units in the \code{unitobjects} slot of \code{panelPomp} objects.
#'
#' Help me!
#'
#' S4 method.
#'
# Can't @inheritParams coef because coef's argument is "object" not "x" (which is length's argument)
#' @param x A \code{panelPomp} object.
#'
#' @export
#'
setMethod(
  f = "length",
  signature = signature(x = "panelPomp"),
  definition = function(x) {
    length(unitobjects(object = x))
  }
)# END setMethod




### mif2 method for panelPomp signature
#' Apply the \code{mif2} algorithm to a \code{panelPomp} object.
#'
#' Mystery of mysteries
#'
#' S4 method.
#'
#' @inheritParams coef,panelPomp-method 
#' @inheritParams pomp::mif2
#' @param shared.start shared.arg.
#' @param specific.start specific.arg.
#' @param rw.sd An unevaluated expression of the form \code{quote(rw.sd())} to be used for all panel units. If a \code{list} of such expressions of the same length as the \code{object} argument is provided, each list element will be used for the corresponding panel unit.
#' @param cooling.fraction.50 cooling.fraction.50 (seems to cause an error if documentation inherited from 'pomp' package)
#' @param transform transform (seems to cause an error if documentation inherited from 'pomp' package)
#'
#' @export
#'
setMethod(
  f = "mif2",
  signature = signature(object = "panelPomp"),
  definition = function(object,
                        shared.start,
                        specific.start,
                        Np,
                        Nmif = 1,
                        cooling.type = c("hyperbolic", "geometric"),
                        cooling.fraction.50,
                        transform = FALSE,
                        rw.sd,
                        verbose = getOption("verbose"),
                        ...) {

      ep <- paste0("in ",sQuote("mif2"),": ")
    
    # If no starting values are specified, try using the pParams slot
    if (missing(shared.start)) shared.start <- coef(object)$shared
    if (missing(specific.start)) specific.start <- coef(object)$specific
    
    if (length(shared.start)==0 & nrow(specific.start)==0){
      stop(
        sQuote("mif2"), " error: ", "non-empty ", sQuote("shared.start"), "or ", 
        sQuote("specific.start"), " must be specified if ", sQuote("coef(object)")," is empty",
        call.=FALSE
      )
    }
    # If the pParams slot is not empty, check that the shared and specific structure of any 
    # provided starting values match the pParams slot
    if ((length(coef(object)$shared) + nrow(coef(object)$specific)) > 0){
      if (
        !identical(character(0), setdiff(x = names(coef(object)$shared), y = names(shared.start)))
        &
        !(is.null(names(coef(object)$shared)) & is.null(names(shared.start)))
      ){
        stop(
          sQuote("mif2"), " error: ", "names of ", sQuote("shared.start"), " must match those of ", 
          sQuote("coef(object)$shared"), call.=FALSE
        )
      }
      if (
        !identical(character(0), setdiff(x = rownames(coef(object)$specific), y = rownames(specific.start)))
        &
        !(is.null(rownames(coef(object)$specific)) & is.null(rownames(specific.start)))
      ){
        stop(
          sQuote("mif2"), " error: ", "rownames of ", sQuote("specific.start"), " must match those of ", 
          sQuote("coef(object)$specific"), call.=FALSE
        )
      }
      if (!identical(x = colnames(coef(object)$specific), y = colnames(specific.start))){
        stop(
          sQuote("mif2"), " error: ", "colnames of ", sQuote("specific.start"), " must be identical to those of ", 
          sQuote("coef(object)$specific"), call.=FALSE
        )
      }
    }
    if (missing(Np)) {
      stop("Missing 'Np' argument.")
    }
    if (missing(cooling.fraction.50)) {
      stop("Missing 'cooling.fraction.50' argument.")
    }
    if (missing(rw.sd)) {
      stop(ep,"missing ",sQuote("rw.sd")," argument.",call.=FALSE)
    }
    # Check that all parameters in the pomp objects have been provided either as shared or specific ...
    if(!all(names(coef(unitobjects(object)[[1]])) %in% c(names(shared.start), rownames(specific.start)))) 
      stop("At least one 'pomp' parameter needs to be added to the (shared. or specific.) start argument")
    # ... and viceversa.
    if(!all(c(names(shared.start), rownames(specific.start))  %in% names(coef(unitobjects(object)[[1]]))))
      stop("At least one parameter in the (shared. or specific.) start argument is not being used")
    pmif2.internal(
      object,
      pstart = list(shared = shared.start, specific = specific.start),
      Np = Np,
      Nmif = Nmif,
      cooling.type = cooling.type,
      cooling.fraction.50 = cooling.fraction.50,
      transform = transform,
      prw.sd = rw.sd,
      ...
    )# END CALL pmif2.internal
  } # END FN definition arg
) # END setMethod




### pfilter method for panelPomp signature
#' Apply a particle filter to \code{panelPomp} objects.
#'
#' S4 method.
#'
#' S4 method.
#'
#' @inheritParams coef,panelPomp-method 
#' @inheritParams panelPomp,list-method
#' @inheritParams pomp::mif2
#' @param ... A \code{ptol} argument with unit-specific tolerances can be passed as a named numeric vector with names matching \code{names(unitobjects(object))}.
#'
#' @export
#'
setMethod(
  f = "pfilter",
  signature = signature(object = "panelPomp"),
  definition =
    function(object,
             shared,
             specific,
             Np,
             verbose = getOption("verbose"),
             ...) {
      # Deal with missing arguments
      if (missing(object)) {
        "Missing object argument."
      }
      if (missing(Np)) {
        "Missing Np argument."
      }      
      if (missing(shared)) {
        if (identical(object@pParams$shared, numeric(0))) {
          "Missing shared argument."
        } else {
          shared <- object@pParams$shared
        }
      }
        if (missing(specific)) {
          if (identical(object@pParams$specific,
                        array(
                          data = numeric(0),
                          dim = c(0, length(object)),
                          dimnames = list(NULL, names(unitobjects(object = object)))
                        ))) {
            "Missing specific argument."
          } else {
            specific <- object@pParams$specific
          }
        }
        pParams <- list(shared = shared, specific = specific)
        #      # If missing params, use params in pomp objects
        #      if (missing(params)){
        #        params <- NULL
        #        for (i.u in 1:length(object@unit.objects)){
        #          params <- cbind(
        #            params,
        #            coef(object@unit.objects[i.u][[1]])
        #          )
        #        }# end for i.u
        #        params <- matrix(
        #          params,
        #          nrow = dim(params)[1],
        #          ncol = dim(params)[2],
        #          dimnames = list(
        #            names(coef(object@unit.objects[1][[1]])),
        #            NULL
        #          )
        #        )
        #      }# END IF missing params
        # Check that all parameters in the pomp objects have been provided either as shared or specific
        if(!all(names(coef(unitobjects(object)[[1]])) %in% c(names(shared), rownames(specific)))) 
          stop("At least one 'pomp' parameter needs to be added to the shared or specific argument")
        #if(!all(c(names(shared), rownames(specific))  %in% names(coef(unitobjects(object)[[1]]))))
        #  stop("At least one parameter in the shared or specific argument is not being used")
        pPfilter.internal(
          object = object,
          pParams = pParams,
          Np = Np,
          verbose = verbose,
          ...
        )
      }
) # END setMethod




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
