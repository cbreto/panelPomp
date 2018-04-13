## MIF2 algorithm codes

#' @include pfilter_methods.R
NULL

#' @title PIF: Panel iterated filtering
#' @description Tools for applying iterated filtering algorithms to panel data.
#' The panel iterated filtering of Breto et al. (2018) extends to 
#' panel models the improved iterated filtering algorithm (Ionides et al., 
#' 2015) for estimating parameters of a partially observed Markov process.
#' Iterated filtering algorithms rely on extending a partially observed Markov 
#' process model of interest by introducing random perturbations to the model 
#' parameters. The space where the original parameters live is then explored 
#' at each iteration by running a particle filter. Convergence to a maximum 
#' likelihood estimate has been established for appropriately constructed 
#' procedures that iterate this search over the parameter space while 
#' diminishing the intensity of perturbations (Ionides et al. 2006, 2011, 2015).
#' @inheritParams mif2,panelPomp-method 
#' @inheritParams coef,panelPomp-method 
#' @inheritParams pomp::mif2
#' @param object An object of class \code{panelPomp} or inheriting class.
#' @param shared.start named numerical vector; the starting guess of the shared
#'  parameters.
#' @param specific.start matrix with row parameter names and column 
#' unit names; the starting guess of the specific parameters.
#' @param start A \code{list} with starting guess of length 2 with elements 
#' named \code{shared} and \code{specific}.
#' @param rw.sd An unevaluated expression of the form \code{quote(rw.sd())} to 
#' be used for all panel units. If a \code{list} of such expressions of the 
#' same length as the \code{object} argument is provided, each list element 
#' will be used for the corresponding panel unit.
#' @param cooling.fraction.50 cooling.fraction.50 (seems to cause an error if 
#' documentation inherited from 'pomp' package).
#' @param transform logical; if TRUE, optimization is performed on the 
#' estimation scale (see \code{pomp} documentation).
#' @name mif2
#' @references \breto2018
#' 
#' \ionides2006
#' 
#' \ionides2011
#' 
#' \ionides2015
#' 
#' \king2015
#' @family panelPomp workhorse functions
#' @seealso \pkg{pomp}'s mif2 at \link[=mif2,pomp-method]{mif2}, 
#' \link{panel_loglik}
NULL

#' @rdname mif2
#' @export
setClass(
  'mif2d.ppomp',
  contains = 'pfilterd.ppomp',
  slots = c(
    Nmif = 'integer',
    prw.sd = 'list',
    cooling.type = 'character',
    cooling.fraction.50 = 'numeric',
    transform = 'logical',
    pconv.rec = 'matrix',
    pconv.rec.array = 'array'),
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

# pmif2 algorithm internal functions
mif2.internal <- function (object, Nmif, start, Np, rw.sd, transform = FALSE, 
  cooling.type, cooling.fraction.50,
  tol = 1e-17, verbose = FALSE, .ndone = 0L,
  ...) {
  # BEGIN DEBUG
  #require(panelPomp)
  #pompExample(gompertz)
  #two.obs.gompertz <- gompertz
  #time(two.obs.gompertz) <- time(gompertz)[1:2]
  #two.obs.gompertz@data <-
  #  gompertz@data[, 1:length(time(two.obs.gompertz)), drop = FALSE]
  #object <-
  #  panelPomp(object = list(unit1 = two.obs.gompertz, unit2 = two.obs.gompertz))
  #Nmif <- 2
  #start <-
  ##  # start with only one shared parameter
  ##  list(
  ##    shared = c(tau = 0.7),
  ##    specific = array(
  ##      data = c(11, 1, 0.1, 0.5, 22, 2, 0.2, 0.55),
  ##      dim = c(4, length(object)),
  ##      dimnames = list(c("X.0", "K", "r", "sigma"),
  ##                      names(unitobjects(object)))
  ##    )
  ##  )
  ##  
  ## #  # start with both shared and specific parameters
  ## #  list(
  ##    shared = c(tau = 0.7, r = 0.1, sigma = 0.5),
  ##    specific = array(
  ##      data = c(11, 1, 22, 2),
  ##      dim = c(2, length(object)),
  ##      dimnames = list(c("X.0", "K"),
  ##                      names(unitobjects(object)))
  ##   )
  ##  )
  ##
  #  # start with only one specific parameter
  #  list(
  #    shared = c(tau = 0.7, r = 0.1, sigma = 0.5, K = 1),
  #    specific = array(
  #      data = c(11, 22),
  #      dim = c(1, length(object)),
  #      dimnames = list(c("X.0"),
  #                     names(unitobjects(object)))
  #    )
  #  )
  #
  ##  # start with no specific parameter
  ##  list(
  ##    shared = c(tau = 0.7, r = 0.1, sigma = 0.5, K = 1, X.0 = 11),
  ##    specific = array(
  ##      data = numeric(0),
  ##      dim = c(0, length(object)),
  ##      dimnames = list(NULL,
  ##                      names(unitobjects(object)))
  ##    )
  ##  )
  ##
  ###  # start with no shared parameter
  ##  list(shared = numeric(0),
  ##       specific = array(
  ##         data = c(11, 1, 0.71, 0.1, 0.5,
  ##                  22, 2, 0.72, 0.2, 0.55),
  ##         dim = c(5, length(object)),
  ##         dimnames = list(c("X.0", "K", "tau", "r", "sigma"),
  ##                         names(unitobjects(object)))
  ##         )
  ##       )
  #Np <- 50
  #rw.sd <- substitute(pomp::rw.sd(tau = 0.02, X.0 = ivp(0.2)))
  #transform <- TRUE
  #cooling.type <- "geometric"
  #cooling.fraction.50 <- 0.5
  #.ndone <- 0L
  ##  # END DEBUG
  
  # Error prefix
  ep <- wQuotes("in ''mif2'': ")
  et <- wQuotes(" (panelPomp:::mif2.internal)")
  
  # PRELIMS & BASIC CHECKS
  U <- as.integer(length(object))
  Nmif <- as.integer(Nmif)
  # Check rw.sd: if it is not a list, make it one
  if (!is.list(rw.sd)) rw.sd <- rep(list(rw.sd), U)
  shnames <- names(start$shared)
  spnames <- rownames(start$specific)
  
  ########################################################
  # Initialize objects
  ########################################################
  
  # Initialize pParamMatrix
  pParamMatrix <- array(
    data = start$shared,
    dim = c(length(shnames), Np),
    dimnames = list(
      variable = shnames, 
      rep = NULL
    )
  )
  # Initialize pparamArray
  pparamArray <- array(
    data = apply(start$specific, 2, rep, times = Np),
    dim = c(length(spnames), Np, U),
    dimnames = list(
      variable = spnames,
      rep = NULL,
      unit = names(unitobjects(object))
    )
  )
  # Initialize pconv.rec and pconv.rec.array
  pconv.rec <- array(
    data = numeric(0),
    dim = c(Nmif + 1, length(start$shared) + 2),
    dimnames = list(
      iteration = seq.int(.ndone, .ndone + Nmif),
      variable = c('loglik', 'nfail', names(start$shared))
    )
  )
  pconv.rec[1L,-c(1:2)] <- start$shared
  
  pconv.rec.array <- array(
    data = numeric(0),
    dim = c(Nmif + 1, dim(start$specific)[1] + 3, U),
    dimnames = list(
      iteration = seq.int(.ndone, .ndone + Nmif),
      variable = c('loglik', 'unitNfail', 'unitLoglik', 
        dimnames(start$specific)[[1]]),
      unit = names(unitobjects(object))
    )
  )
  pconv.rec.array[1L, -c(1:3), ] <- pparamArray[, 1 ,]
  # Initialize output
  output <- vector(mode="list",length=U)
  # nameoutput
  names(output) <- names(unitobjects(object))
  
  ###########################################################
  # LOOP OVER MIF ITERATIONS AND PANEL UNITS
  ###########################################################
  
  unit.seq <- seq_len(U)
  
  for (mifiter in seq_len(Nmif)) {
    for (unit in unit.seq) {
      # Create a (n updated) paramMatrix to pass to mif2 on next panel unit
      updated.paramMatrix <- rbind(pParamMatrix, pparamArray[, , unit])
      # Here, we want to drop the unit dimension but, if there was only one 
      # specific parameter, R will then have dropped its name, which we fix by
      rownames(updated.paramMatrix) <- c(
        rownames(pParamMatrix), rownames(pparamArray))
      
      output[[unit]] <- tryCatch(
        pomp::mif2(
          object = object@unit.objects[[unit]],
          Nmif = 1,
          Np = Np,
          rw.sd = rw.sd[[unit]],
          transform = transform,
          cooling.type = cooling.type,
          cooling.fraction.50 = cooling.fraction.50,
          tol = tol,
          verbose = verbose,
          .paramMatrix = updated.paramMatrix,
          .indices = seq.int(Np),
          .ndone = mifiter-1
        ),
        error = function (e) {
          stop(ep,"pomp's ",sQuote("mif2")," error message: ",conditionMessage(e),
            et,call.=FALSE)
        }
      )
      
      # Update (panelPomp) pParamMatrix with (pomp) paramMatrix ...
      pParamMatrix <- output[[unit]]@paramMatrix[shnames, , drop = FALSE]
      # ... and update pparamArray:
      # first for the current unit ...
      pparamArray[spnames, , unit] <- output[[unit]]@paramMatrix[spnames, , drop = FALSE]
      # ... then, resample all other units using the mif2d.pomp indices
      pparamArray[spnames, , -unit] <- pparamArray[spnames, output[[unit]]@indices, -unit, drop = FALSE]
      
      # Cleaning up: remove the paramMatrix slot from the mif2d.pomp object to minimize memory requirements
      output[[unit]]@paramMatrix <- array(data = numeric(0), dim = c(0, 0))
      # Finish by passing the updates onto pconv.rec and pconv.rec.array when appropriate
      if (unit == tail(x = unit.seq, n = 1)) {
        # ... pconv.rec ...
        pconv.rec[mifiter + 1, -c(1:2)] <- apply(X = pParamMatrix, MARGIN = 1, FUN = mean)
        pconv.rec[mifiter, "loglik"] <- sum(sapply(X = output, FUN = logLik))
        pconv.rec[mifiter, "nfail"] <- sum(sapply(X = output, slot, "nfail"))
        # ... and pconv.rec.array
        if (!is.null(spnames)) {
          pconv.rec.array[mifiter + 1, -c(1:3), ] <- apply(X = pparamArray, MARGIN = c(1,3), FUN = mean)
          pconv.rec.array[mifiter, "loglik", ] <- sum(sapply(X = output, FUN = logLik))
          pconv.rec.array[mifiter, "unitLoglik", ] <- sapply(X = output, FUN = logLik)
          pconv.rec.array[mifiter, "unitNfail",] <- sapply(X = output, FUN = slot, "nfail")
        }
      }
    }
  }
  
  #######################################################
  # BEFORE RETURNING OUTPUT, PERFORM FINAL ARRANGEMENTS #
  #######################################################
  
  # Extract loglikelihoods
  unit.logliks <- sapply(X = output, FUN = logLik)
  ploglik <- sum(unit.logliks)
  # create pParams slot from last mif iteration values in pconv.rec
  pParams <- list()
  pParams$shared <- pconv.rec[nrow(pconv.rec), -c(1:2)]
  # Here, we want to drop the iteration dimension but, if there was only one 
  # shared parameter, R will then have dropped its name, which we fix by
  names(pParams$shared) <- dimnames(pconv.rec)$variable[-c(1:2)]
  pParams$specific <- aperm(
    a = pconv.rec.array[nrow(pconv.rec), -c(1:3), , drop = FALSE],
    perm = c(2, 3, 1)
  )
  dim(pParams$specific) <- dim(pParams$specific)[1:2]
  dimnames(pParams$specific) <- list( 
    variable = colnames(pconv.rec.array)[-c(1:3)],
    unit = dimnames(pconv.rec.array)$unit
  )
  # To have unit-specific tolerances, one could use something like:
  #ptol <- sapply(output, slot, "tol")
  
  # Return the end "mif2d.ppomp" object
  return(
    new(
      Class = "mif2d.ppomp",
      # panelPomp
      unit.objects = output,
      shared = pParams$shared,
      specific = pParams$specific,
      # pfilterd.ppomp
      Np = Np,
      ploglik = ploglik,
      tol = tol,
      unit.logliks = unit.logliks,      
      # mif2d.ppomp
      Nmif = Nmif,
      prw.sd = rw.sd,
      cooling.type = cooling.type,
      cooling.fraction.50 = cooling.fraction.50,
      transform = transform,
      pconv.rec = pconv.rec,
      pconv.rec.array = pconv.rec.array
    )
  )
}

## mif2,panelPomp-method
#' @rdname mif2
#' @export
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
    ep <- wQuotes("in ''mif2'': ")
    et <- wQuotes(" (''mif2,panelPomp-method'')")
    ## check for start (i.e., params) format
    if (!missing(start) && is.numeric(start)) start <- pParams(start)
    
    if (!missing(shared.start)&&!missing(specific.start)&&!missing(start)) 
      stop(wQuotes(ep,"specify either ''start'' only, ''start'' and ",
        "''shared.start'', or ''start'' and ''specific.start''.",
        et),call.=FALSE)
    # Get starting parameter values from 'object,' 'start,' or 
    # 'shared/specific.start'
    if (missing(shared.start)){
      if (!missing(start)) shared.start <- start$shared 
      else shared.start <- object@shared
    } 
    if (missing(specific.start)){
      if (!missing(start)) specific.start <- start$specific 
      else specific.start <- object@specific
    }
    # This causes an unintended stop in panelPomp objects that genuinely 
    # have no shared parameters
    #if (identical(shared.start,numeric(0))) {
    #  stop(ep,"if ",sQuote("object@shared")," is empty, shared parameters
    #       must be specified in either ",sQuote("shared.start"),
    #       " or as part of ",sQuote("start"),".",et,call.=FALSE
    #  )
    #}
    # Obsolete check: valid panelPomps won't have completely empty sp matrix
    #if (identical(specific.start,array(numeric(0),dim=c(0,0)))) {
    #  stop(ep,"if ",sQuote("object@specific")," is empty, specific 
    #       parameters must be specified in either ",sQuote("specific.start"),
    #       " or as part of ",sQuote("start"),".",et,call.=FALSE
    #  )
    #}
    # If the object pParams slot is not empty, check that the shared and 
    # specific structure of any provided starting values match the pParams 
    # slot
    if (!is.null(object@shared)) {
      if (
        !identical(
          character(0),
          setdiff(names(object@shared),names(shared.start))
        )
        &
          !(is.null(names(object@shared))&is.null(names(shared.start)))
      ) {
        stop(wQuotes(ep,"part of ''shared.start'' is not a shared parameter",
          " of ''object''.",et),call.=FALSE)
      }
    }
    if (!is.null(object@specific)){
      if (
        !identical(
          character(0),
          setdiff(rownames(object@specific),rownames(specific.start))
        )
        &
          !(is.null(rownames(object@specific))
            &
              is.null(rownames(specific.start))
          )
      ) {
        stop(wQuotes(ep,"part of ''specific.start'' is not a specific parameter",
          " of ''object''."),et,call.=FALSE)
      }
      if (!identical(
        colnames(object@specific),
        colnames(specific.start))){
        stop(ep,"colnames of ",sQuote("specific")," must be identical to ",
          "those of ",sQuote("object@specific"),".",et,call.=FALSE)
      }
    }
    
    if (missing(Np)) {
      stop(ep,"Missing ",sQuote("Np")," argument.",et,call.=FALSE)
    }
    if (missing(cooling.fraction.50)) {
      stop(ep,"Missing ",sQuote("cooling.fraction.50")," argument.",et,
        call.=FALSE)
    }
    if (missing(rw.sd)) {
      stop(ep,"missing ",sQuote("rw.sd")," argument.",et,call.=FALSE)
    }
    
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
    )
  }
)

#' @rdname mif2
#' @export
setMethod(
  "mif2",
  signature=signature(object="mif2d.ppomp"),
  definition = function (object, Nmif, shared.start, specific.start, Np, rw.sd,
    transform, cooling.type, cooling.fraction.50, tol,
    ...) {
    if (missing(Nmif)) Nmif <- object@Nmif
    if (missing(shared.start)) shared.start <- object@shared
    if (missing(specific.start)) specific.start <- object@specific
    if (missing(Np)) Np <- object@Np    
    if (missing(rw.sd)) rw.sd <- object@prw.sd
    if (missing(transform)) transform <- object@transform
    if (missing(cooling.type)) cooling.type <- object@cooling.type
    if (missing(cooling.fraction.50)) 
      cooling.fraction.50 <- object@cooling.fraction.50
    if (missing(tol)) tol <- object@tol
    
    f <- selectMethod("mif2",signature="panelPomp")
    f(object=object,shared.start=shared.start,specific.start=specific.start,
      Np=Np,Nmif=Nmif,cooling.type=cooling.type,
      cooling.fraction.50=cooling.fraction.50,transform=transform,rw.sd=rw.sd,
      tol=tol,...)
  }
)
