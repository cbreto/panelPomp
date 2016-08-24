#' @include mif2d-ppomp-class.R
NULL

# pmif2 algorithm internal functions
pmif2.internal <- function(object,
                           Nmif,
                           pstart,
                           Np,
                           prw.sd,
                           transform = FALSE,
                           cooling.type,
                           cooling.fraction.50,
                           .ndone = 0L,
                           rand.unit = FALSE,
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
  #pstart <-
  ##  # pstart with only one shared parameter
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
  ## #  # pstart with both shared and specific parameters
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
  #  # pstart with only one specific parameter
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
  ##  # pstart with no specific parameter
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
  ###  # pstart with no shared parameter
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
  #prw.sd <- substitute(pomp::rw.sd(tau = 0.02, X.0 = ivp(0.2)))
  #transform <- TRUE
  #cooling.type <- "geometric"
  #cooling.fraction.50 <- 0.5
  #.ndone <- 0L
  ##  # END DEBUG
  
  # Error prefix
  ep <- paste0("in ", sQuote("panelPomp::mif2"), ": ")
  
  # PRELIMS & BASIC CHECKS
  U <- as.integer(length(object))
  Nmif <- as.integer(Nmif)
  # Check prw.sd: if it is not a list, make it one
  if (!is.list(prw.sd)) prw.sd <- rep(list(prw.sd), U)
  shnames <- names(pstart$shared)
  spnames <- rownames(pstart$specific)
  
  ########################################################
  # Initialize objects
  ########################################################
  
  # Initialize pParamMatrix
  pParamMatrix <- array(
    data = pstart$shared,
    dim = c(length(shnames), Np),
    dimnames = list(
      variable = shnames, 
      rep = NULL
    )
  )
  # Initialize pparamArray
  pparamArray <- array(
    data = apply(pstart$specific, 2, rep, times = Np),
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
    dim = c(Nmif + 1, length(pstart$shared) + 2),
    dimnames = list(
      iteration = seq.int(.ndone, .ndone + Nmif),
      variable = c('loglik', 'nfail', names(pstart$shared))
    )
  )
  pconv.rec[1L,-c(1:2)] <- pstart$shared
  
  pconv.rec.array <- array(
    data = numeric(0),
    dim = c(Nmif + 1, dim(pstart$specific)[1] + 3, U),
    dimnames = list(
      iteration = seq.int(.ndone, .ndone + Nmif),
      variable = c('loglik', 'unitNfail', 'unitLoglik', dimnames(pstart$specific)[[1]]),
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
  
  for (mifiter in seq_len(Nmif)) {
    unit.seq <- if(rand.unit==T) sample(seq_len(U)) else seq_len(U)
    for (unit in unit.seq) {
      ## DEBUG
      # i.loop <- as.integer(1)
      # i.loop <- as.integer(2)
      # i.loop <- as.integer(3)
      # i.loop <- as.integer(4)
      
      # Create a (n updated) paramMatrix to pass to mif2 on next panel unit
      updated.paramMatrix <- rbind(pParamMatrix, pparamArray[, , unit])
      # Here, we want to drop the unit dimension but, if there was only one specific parameter,
      # R will then have dropped its name, which we fix by
      rownames(updated.paramMatrix) <- c(rownames(pParamMatrix), rownames(pparamArray))
      
      # use pomp::mif2 the first time and pomp::continue after that
      if (mifiter==1) {
        output[[unit]] <- tryCatch(
          pomp::mif2(
            object = object@unit.objects[[unit]],
            Np = Np,
            Nmif = 1,
            cooling.type = cooling.type,
            cooling.fraction.50 = cooling.fraction.50,
            transform = transform,
            rw.sd = prw.sd[[unit]],
            .paramMatrix = updated.paramMatrix,
            .indices = seq.int(Np)
          ),
          error = function (e) {
            stop(ep,"error in ",sQuote("pomp::mif2"),": ",
                 conditionMessage(e),call.=FALSE)
          }
        )
      } else {
        # ... and run iterated filtering by continuing mif2d.pomp objects)
        output[[unit]] <- tryCatch(
          pomp::continue(object = output[[unit]],
                         .paramMatrix = updated.paramMatrix,
                         .indices = seq.int(Np)),
          error = function (e) {
            stop(ep,"error in ",sQuote("pomp::continue"),": ",
                 conditionMessage(e),call.=FALSE)
          }
        )
      }
      
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
        pconv.rec[mifiter + 1, "loglik"] <- sum(sapply(X = output, FUN = logLik))
        pconv.rec[mifiter + 1, "nfail"] <- sum(sapply(X = output, slot, "nfail"))
        # ... and pconv.rec.array
        if (!is.null(spnames)) {
          pconv.rec.array[mifiter + 1, -c(1:3), ] <- apply(X = pparamArray, MARGIN = c(1,3), FUN = mean)
          pconv.rec.array[mifiter + 1, "loglik", ] <- sum(sapply(X = output, FUN = logLik))
          pconv.rec.array[mifiter + 1, "unitLoglik", ] <- sapply(X = output, FUN = logLik)
          pconv.rec.array[mifiter + 1, "unitNfail",] <- sapply(X = output, FUN = slot, "nfail")
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
  # Here, we want to drop the iteration dimension but, if there was only one shared 
  # parameter, R will then have dropped its name, which we fix by
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
  ptol <- sapply(output, slot, "tol")
  
  # Return the end "mif2d.ppomp" object
  return(
    new(
      Class = "mif2d.ppomp",
      # panelPomp
      unit.objects = output,
      pParams = pParams,
      # pfilterd.ppomp
      Np = Np,
      ploglik = ploglik,
      ptol = ptol,
      unit.logliks = unit.logliks,      
      # mif2d.ppomp
      Nmif = Nmif,
      prw.sd = prw.sd,
      cooling.type = cooling.type,
      cooling.fraction.50 = cooling.fraction.50,
      transform = transform,
      pconv.rec = pconv.rec,
      pconv.rec.array = pconv.rec.array
    )
  )
}
