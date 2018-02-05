#' @include panelPomp-internal.R
NULL


### 'coef' method for panelPomp signature
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
  definition = function (object) {
    pmat <- object@pParams$specific
    c(
      object@pParams$shared,
      setNames(
        as.numeric(pmat),
        outer(rownames(pmat),colnames(pmat),sprintf,fmt="%s[%s]")
        )
    )
  }
)

setMethod(
  "coef<-",
  signature=signature(object="panelPomp"),
  definition=function (object, ..., value) {
    nn <- grep("^.+\\[.+?\\]$",names(value),perl=TRUE,value=TRUE)
    pp <- sub(pattern="^(.+?)\\[.+?\\]$",replacement="\\1",x=nn,perl=TRUE)
    uU <- names(object@unit.objects)
    pU <- sort(unique(pp))
    object@pParams$specific <- array(dim=c(length(pU),length(uU)),
                                     dimnames=list(param=pU,unit=uU))
    pvec <- setNames(numeric(length(object@pParams$specific)),
                     outer(pU,uU,sprintf,fmt="%s[%s]"))
    unitpar <- intersect(names(value),names(pvec))
    sharedpar <- setdiff(names(value),unitpar)
    pvec[unitpar] <- value[unitpar]
    object@pParams$specific[,] <- pvec
    object@pParams$shared <- value[sort(sharedpar)]
    object
  }
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



### mif2 method for panelPomp signature
#' IF2: Maximum likelihood via panel iterated, perturbed Bayes maps
#'
#' Extension to panel models of the improved iterated filtering algorithm (Ionides et al., 2015) for estimating parameters of a partially observed Markov process.
#' Iterated filtering algorithms rely on extending a partially observed Markov process model of interest by introducing random perturbations to the model parameters. 
#' The space where the original parameters live is then explored at each iteration by running a particle filter.
#' Convergence to a maximum likelihood estimate has been established for appropriately constructed procedures that iterate this search over the parameter space while diminishing the intensity of perturbations (Ionides et al. 2006, 2011, 2015).
#'
#' @inheritParams coef,panelPomp-method 
#' @inheritParams pomp::mif2
#' @param shared.start named numerical vector; the starting guess of the shared parameters.
#' @param specific.start matrix with row parameter names and column 
#' unit names; the starting guess of the specific parameters.
#' @param start A \code{list} with starting guess of length 2 with elements named \code{shared} and \code{specific}.
#' @param rw.sd An unevaluated expression of the form \code{quote(rw.sd())} to be used for all panel units. If a \code{list} of such expressions of the same length as the \code{object} argument is provided, each list element will be used for the corresponding panel unit.
#' @param cooling.fraction.50 cooling.fraction.50 (seems to cause an error if documentation inherited from 'pomp' package)
#' @param transform logical; if TRUE, optimization is performed on the estimation scale (see \code{pomp} documentation).
#'
#' @export
#'
setMethod(
  "mif2",
  signature=signature(object="panelPomp"),
  definition = function (object, Nmif = 1, shared.start, specific.start, 
                         start = list(
                           shared = shared.start, 
                           specific = specific.start
                         ),
                         Np, rw.sd, transform = FALSE, 
                         cooling.type = c("hyperbolic", "geometric"), 
                         cooling.fraction.50,
                         tol = 1e-17,
                         verbose = getOption("verbose"), 
                         ...) {
    
    ep <- paste0(sQuote("panelPomp::mif2")," error: ")
    
    if (!missing(shared.start)&&!missing(specific.start)&&!missing(start)) 
      stop(ep,"specify either ",sQuote("start")," only, ",sQuote("start"),
           " and ",sQuote("shared.start")," , or ",sQuote("start")," and ",
           sQuote("specific.start"),".",call.=FALSE
      )
    
    # Get starting parameter values from 'object,' 'start,' or 
    # 'shared/specific.start'
    if (missing(shared.start)){
      if (!missing(start)) shared.start <- start$shared 
      else shared.start <- object@pParams$shared
    }
    if (missing(specific.start)){
      if (!missing(start)) specific.start <- start$specific 
      else specific.start <- object@pParams$specific
    }
    
    # This causes an unintended stop in panelPomp objects that genuinely have no shared parameters
    #if (identical(shared.start,numeric(0))) {
    #  stop(ep,"if ",sQuote("object@pParams$shared")," is empty, shared parameters
    #       must be specified in either ",sQuote("shared.start"),
    #       " or as part of ",sQuote("start"),".",call.=FALSE
    #  )
    #}
    if (identical(specific.start,array(numeric(0),dim=c(0,0)))) {
      stop(ep,"if ",sQuote("object@pParams$specific")," is empty, specific 
           parameters must be specified in either ",sQuote("specific.start"),
           " or as part of ",sQuote("start"),".",call.=FALSE
      )
    }
    # If the object pParams slot is not empty, check that the shared and 
    # specific structure of any provided starting values match the pParams slot
    if (!is.null(object@pParams$shared)) {
      if (
        !identical(
          character(0),
          setdiff(names(object@pParams$shared),names(shared.start))
        )
        &
        !(is.null(names(object@pParams$shared))&is.null(names(shared.start)))
      ) {
        stop(ep, "names of ", sQuote("shared.start"), " must match those of ", 
             sQuote("object@pParams$shared"),".", call.=FALSE
        )
      }
    }
    if (!is.null(object@pParams$specific)){
      if (
        !identical(
          character(0),
          setdiff(rownames(object@pParams$specific),rownames(specific.start))
        )
        &
        !(is.null(rownames(object@pParams$specific))
          &
          is.null(rownames(specific.start))
        )
      ){
        stop(ep, "rownames of ", sQuote("specific.start"), " must match those of ", 
             sQuote("object@pParams$specific"),".", call.=FALSE
        )
      }
      if (!identical(x = colnames(object@pParams$specific), y = colnames(specific.start))){
        stop(ep, "colnames of ", sQuote("specific"), " must be identical to those of ", 
             sQuote("object@pParams$specific"),".", call.=FALSE
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
    if(!all(names(unitobjects(object)[[1]]@params) %in% c(names(shared.start), rownames(specific.start)))) 
      stop("At least one 'pomp' parameter needs to be added to the (shared. or specific.) start argument")
    # ... and viceversa.
    if(!all(c(names(shared.start), rownames(specific.start))  %in% names(unitobjects(object)[[1]]@params)))
      stop("At least one parameter in the (shared. or specific.) start argument is not being used")
    mif2.internal(
      object,
      Nmif=Nmif,
      start=list(shared=shared.start,specific=specific.start),
      Np=Np,
      rw.sd=rw.sd,
      transform=transform,
      cooling.type=cooling.type,
      cooling.fraction.50=cooling.fraction.50,
      tol=tol,
      verbose=verbose,
      ...
    )# END CALL mif2.internal
  } # END FN definition arg
) # END setMethod




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
  definition = function (x) names(x@unit.objects)
)


#' Redefine shared/specific configuration of a panelPomp object.
#'
#' Extract shared and specific parameter values from a panelPomp and use them to create a new panelPomp object with a new configuration of shared and specific parameters.
#'
#' @param object panelPomp object.
#' @param shared character; names of shared parameters. Those which were not originally shared are copied from the specific parameters for the first panel unit. Defaults to \code{NULL}, which implies all parameters are specific.
#'
#' @export
#'
setMethod(
  f = "panelPomp",
  signature = signature(object = "panelPomp"),
  definition = function (object, shared = NULL) {
    parnames <- c(names(object@pParams$shared),row.names(object@pParams$sp))
    stopifnot(all(shared%in%parnames))
    sp <- parnames[!parnames%in%shared]
    # make matrix from object@pParams$sh that can be rbinded to object@pParams$sp
    sh0 <- names(object@pParams$shared)
    not.in.sp0 <- matrix(
      object@pParams$shared,nrow=length(sh0),ncol=length(object),
      dimnames=list(sh0,names(object))
    )
    all.sp <- rbind(object@pParams$specific,not.in.sp0)
    stopifnot(!as.logical(anyDuplicated(row.names(all.sp))))
    # make vector from object@pParams$sp[1,] that can be c()d to object@pParams$sh
    all.sh <- c(object@pParams$shared,object@pParams$specific[,1])
    shs <- all.sh[shared]
    sps <- all.sp[sp,]
    panelPomp(as(object,"list"),shared=shs,specific=sps)
  }
)



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
#' @param tol filtering tolerance for all units.
#' @param ... additional arguments, passed to the \code{pfilter} method of \pkg{pomp}.
#' @export
#'
setMethod(
  "pfilter",
  signature=signature(object="panelPomp"),
  definition = function(object, shared, specific, params, Np, 
                        tol = 1e-17,
                        verbose = getOption("verbose"),
                        ...) {
    ep <- paste0(sQuote("panelPomp::pfilter")," error: ")
    
    if (!missing(shared) && !missing(specific) && !missing(params)) 
      stop(ep,"specify either ",sQuote("params")," only, ",sQuote("params"),
           " and ",sQuote("shared")," , or ",sQuote("params")," and ",
           sQuote("specific"),".",call.=FALSE
      )
    
    # Get starting parameter values from 'object,' 'start,' or 'params'
    if (missing(shared)){
      if (!missing(params)) shared <- params$shared 
      else shared <- object@pParams$shared
    } 
    if (missing(specific)){
      if (!missing(params)) specific <- params$specific 
      else specific <- object@pParams$specific
    }
    
    # This causes an unintended stop in panelPomp objects that genuinely have no shared parameters      
    #if (identical(shared,numeric(0))) {
    #  stop(ep,"if ",sQuote("object@pParams$shared")," is empty, shared 
    #       parameters must be specified in either ",sQuote("shared"),
    #       " or as part of ",sQuote("params"),".",call.=FALSE
    #  )
    #}
    if (identical(specific,array(numeric(0),dim=c(0,0)))) {
      stop(ep,"if ",sQuote("object@pParams$specific")," is empty, specific 
           parameters must be specified in either ",sQuote("specific"),
           " or as part of ",sQuote("params"),".",call.=FALSE
      )
    }
    # If the pParams slot is not empty, check that the shared and specific structure of any 
    # provided starting values match the pParams slot
    if (!is.null(object@pParams$shared)){
      if (
        !identical(
          character(0),
          setdiff(names(object@pParams$shared),names(shared))
        )
        &
        !(is.null(names(object@pParams$shared))&is.null(names(shared)))
      ) {
        stop(ep, "names of ", sQuote("shared"), " must match those of ", 
             sQuote("object@pParams$shared"),".", call.=FALSE
        )
      }
    }
    if (!is.null(object@pParams$specific)){
      if (
        !identical(
          character(0),
          setdiff(rownames(object@pParams$specific),
                  rownames(specific))
        )
        &
        !(
          is.null(rownames(object@pParams$specific))
          &
          is.null(rownames(specific))
        )
      ) {
        stop(ep,"rownames of ",sQuote("specific")," must match those of ", 
             sQuote("object@pParams$specific"),".",call.=FALSE
        )
      }
      if (!identical(x = colnames(object@pParams$specific), y = colnames(specific))){
        stop(ep, "colnames of ", sQuote("specific"), " must be identical to those of ", 
             sQuote("object@pParams$specific"),".", call.=FALSE
        )
      }
    }
    if (missing(Np)) stop(ep,"Missing 'Np' argument.")
    # Check that all parameters in the pomp objects have been provided either as shared or specific ...
    if(!all(names(unitobjects(object)[[1]]@params) %in% 
      c(names(shared), rownames(specific))))
      stop(ep, "At least one 'pomp' parameter needs to be added to the (shared. or specific.) 
             start argument.")
    # ... and viceversa.
    if(!all(c(names(shared), rownames(specific))  %in% names(unitobjects(object)[[1]]@params)))
      stop(ep, "At least one parameter in the (shared. or specific.) start argument is not 
             being used.")
    
    pfilter.internal(
      object = object,
      params = list(shared = shared, specific = specific),
      Np = Np,
      tol = tol,
      verbose = verbose,
      ...
    )
  }
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
              shared = x@pParams$shared,
              specific = x@pParams$specific[, 1:U]
    )
  }
)
