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
#' @param data An object of class \code{panelPomp} or inheriting class.
#' @param shared.start named numerical vector; the starting guess of the shared parameters.
#' @param specific.start matrix with row parameter names and column unit names;
#' the starting guess of the specific parameters.
#' @param start A named numeric vector of parameters at which to start the IF2 procedure.
#' @param block A logical variable determining whther to carry out block
#' resampling of unit-specific parameters.
#' @param rw.sd An unevaluated expression of the form \code{quote(rw.sd())} to
#' be used for all panel units. If a \code{list} of such expressions of the
#' same length as the \code{object} argument is provided, each list element
#' will be used for the corresponding panel unit.
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
    pconv.rec = 'matrix',
    pconv.rec.array = 'array',
    block = 'logical'),
  prototype = prototype(
    Nmif = integer(0),
    prw.sd = list(),
    cooling.type = character(0),
    cooling.fraction.50 = numeric(0),
    pconv.rec = array(data = numeric(0), dim = c(0, 0)),
    pconv.rec.array = array(data = numeric(0), dim = c(0, 0, 0)),
    block = logical(0)
  )
)

# pmif2 algorithm internal functions
mif2.internal <- function (object, Nmif, start, Np, rw.sd, cooling.type,
                           cooling.fraction.50, block, verbose = FALSE,
                           .ndone = 0L, ...) {
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

  if (!setequal(names(object@unit.objects),colnames(start$specific)))
    stop(ep,wQuotes("specific parameter column-names must match the names of the units"),call.=FALSE)
  start$specific <- start$specific[,names(object@unit.objects),drop=FALSE]

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
    dim = c(Nmif + 1, length(start$shared) + 1),
    dimnames = list(
      iteration = seq.int(.ndone, .ndone + Nmif),
      variable = c('loglik', names(start$shared))
    )
  )
  pconv.rec[1L,-1L] <- start$shared

  pconv.rec.array <- array(
    data = numeric(0),
    dim = c(Nmif + 1, dim(start$specific)[1] + 1, U),
    dimnames = list(
      iteration = seq.int(.ndone, .ndone + Nmif),
      variable = c('unitLoglik',
        dimnames(start$specific)[[1]]),
      unit = names(unitobjects(object))
    )
  )
  pconv.rec.array[1L, -1L, ] <- pparamArray[, 1L,]
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
          object[[unit]],
          Nmif = 1,
          Np = Np,
          rw.sd = rw.sd[[unit]],
          cooling.type = cooling.type,
          cooling.fraction.50 = cooling.fraction.50,
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
      if(!block){
        pparamArray[spnames, , -unit] <- pparamArray[spnames, output[[unit]]@indices, -unit, drop = FALSE]
      }
      # Cleaning up: remove the paramMatrix slot from the mif2d.pomp object to minimize memory requirements
      output[[unit]]@paramMatrix <- array(data = numeric(0), dim = c(0, 0))
      # Finish by passing the updates onto pconv.rec and pconv.rec.array when appropriate
      if (unit == tail(x = unit.seq, n = 1)) {
        # ... pconv.rec ...
        pconv.rec[mifiter + 1, -1L] <- apply(X = pParamMatrix, MARGIN = 1, FUN = mean)
        pconv.rec[mifiter, "loglik"] <- sum(sapply(X = output, FUN = logLik))
        # ... and pconv.rec.array
        if (!is.null(spnames)) {
          pconv.rec.array[mifiter + 1, -1L, ] <- apply(X = pparamArray, MARGIN = c(1L,3L), FUN = mean)
          pconv.rec.array[mifiter, "unitLoglik", ] <- sapply(X = output, FUN = logLik)
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
  pParams$shared <- pconv.rec[nrow(pconv.rec), -1L]
  # Here, we want to drop the iteration dimension but, if there was only one
  # shared parameter, R will then have dropped its name, which we fix by
  names(pParams$shared) <- dimnames(pconv.rec)$variable[-1L]
  pParams$specific <- aperm(
    a = pconv.rec.array[nrow(pconv.rec), -1L, , drop = FALSE],
    perm = c(2, 3, 1)
  )
  dim(pParams$specific) <- dim(pParams$specific)[1:2]
  dimnames(pParams$specific) <- list(
    variable = colnames(pconv.rec.array)[-1L],
    unit = dimnames(pconv.rec.array)$unit
  )

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
      unit.logliks = unit.logliks,
      # mif2d.ppomp
      Nmif = Nmif,
      prw.sd = rw.sd,
      cooling.type = cooling.type,
      cooling.fraction.50 = cooling.fraction.50,
      pconv.rec = pconv.rec,
      pconv.rec.array = pconv.rec.array,
      block = block
    )
  )
}

## mif2,panelPomp-method
#' @rdname mif2
#' @examples
#' p <- panelRandomWalk()
#' mp <- mif2(p,Np=10,rw.sd=rw_sd(X.0=0.2),cooling.fraction.50=0.5,cooling.type="geometric")
#' mp
#' @export
#' @author Carles Breto
setMethod(
  "mif2",
  signature=signature(data="panelPomp"),
  definition = function (data, Nmif = 1, shared.start, specific.start, start,
                         Np, rw.sd, cooling.type = c("hyperbolic", "geometric"),
                         cooling.fraction.50, block = FALSE,
                         verbose = getOption("verbose"), ...) {
    object <- data
    ep <- wQuotes("in ''mif2'': ")
    et <- wQuotes(" (''mif2,panelPomp-method'')")

    if (!missing(start) && (!missing(shared.start) || !missing(specific.start)))
      stop(wQuotes(ep,"specify EITHER ''start'' only OR ''shared.start'' ",
                   "and/or ''specific.start''.",et),call.=FALSE)
    if (missing(start)) {
      start <- list(shared=object@shared,specific=object@specific)
    } else {
     if (is.numeric(start)) start <- pParams(start)
    }
    if (missing(shared.start)) shared.start <- start$shared
    if (missing(specific.start)) specific.start <- start$specific

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
      cooling.type=cooling.type,
      cooling.fraction.50=cooling.fraction.50,
      verbose=verbose,
      block=block,
      ...
    )
  }
)

#' @rdname mif2
#' @examples
#' mmp <- mif2(mp,Np=10,rw.sd=rw_sd(X.0=0.2),cooling.fraction.50=0.5,cooling.type="geometric")
#' mmp
#' @export
#' @author Carles Breto
setMethod(
  "mif2",
  signature=signature(data="mif2d.ppomp"),
  definition = function (data, Nmif, shared.start, specific.start, start,
                         Np, rw.sd, cooling.type, cooling.fraction.50, block,
                         ...) {
    object <- data
    ep <- wQuotes("in ''mif2'': ")
    et <- wQuotes(" (''mif2,mif2d.ppomp-method'')")
    if (missing(Nmif)) Nmif <- object@Nmif
    if (!missing(start) && (!missing(shared.start) || !missing(specific.start)))
      stop(wQuotes(ep,"specify EITHER ''start'' only OR ''shared.start'' ",
                   "and/or ''specific.start''.",et),call.=FALSE)
   if (missing(start)) {
      start <- list(shared=object@shared,specific=object@specific)
    } else {
      if (is.numeric(start)) start <- pParams(start)
    }
    if (missing(shared.start)) shared.start <- start$shared
    if (missing(specific.start)) specific.start <- start$specific
    if (missing(Np)) Np <- object@Np
    if (missing(rw.sd)) rw.sd <- object@prw.sd
    if (missing(cooling.type)) cooling.type <- object@cooling.type
    if (missing(cooling.fraction.50))
      cooling.fraction.50 <- object@cooling.fraction.50
    if (missing(block)) block <- object@block

    f <- selectMethod("mif2",signature="panelPomp")
    f(object,shared.start=shared.start,specific.start=specific.start,Np=Np,
      Nmif=Nmif,cooling.type=cooling.type,
      cooling.fraction.50=cooling.fraction.50,rw.sd=rw.sd,block=block,...)
  }
)
