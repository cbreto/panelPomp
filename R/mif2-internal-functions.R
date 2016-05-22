#' @include mif2d-ppomp-class.R
NULL

# pmif2 algorithm internal functions
pmif2.internal <- function(pPomp.object,
                           Nmif,
                           pstart,
                           Np,
                           prw.sd,
                           transform = FALSE,
                           cooling.type,
                           cooling.fraction.50,
                           .ndone = 0L,
                           ...) {
  # BEGIN DEBUG
  #require(panelPomp)
  #pompExample(gompertz)
  #two.obs.gompertz <- gompertz
  #time(two.obs.gompertz) <- time(gompertz)[1:2]
  #two.obs.gompertz@data <-
  #  gompertz@data[, 1:length(time(two.obs.gompertz)), drop = FALSE]
  #pPomp.object <-
  #  panelPomp(object = list(unit1 = two.obs.gompertz, unit2 = two.obs.gompertz))
  #Nmif <- 2
  #pstart <-
  ##  # pstart with only one shared parameter
  ##  list(
  ##    shared = c(tau = 0.7),
  ##    specific = array(
  ##      data = c(11, 1, 0.1, 0.5, 22, 2, 0.2, 0.55),
  ##      dim = c(4, length(pPomp.object)),
  ##      dimnames = list(c("X.0", "K", "r", "sigma"),
  ##                      names(unitobjects(pPomp.object)))
  ##    )
  ##  )
  ##  
  ## #  # pstart with both shared and specific parameters
  ## #  list(
  ##    shared = c(tau = 0.7, r = 0.1, sigma = 0.5),
  ##    specific = array(
  ##      data = c(11, 1, 22, 2),
  ##      dim = c(2, length(pPomp.object)),
  ##      dimnames = list(c("X.0", "K"),
  ##                      names(unitobjects(pPomp.object)))
  ##   )
  ##  )
  ##
  #  # pstart with only one specific parameter
  #  list(
  #    shared = c(tau = 0.7, r = 0.1, sigma = 0.5, K = 1),
  #    specific = array(
  #      data = c(11, 22),
  #      dim = c(1, length(pPomp.object)),
  #      dimnames = list(c("X.0"),
  #                     names(unitobjects(pPomp.object)))
  #    )
  #  )
  #
  ##  # pstart with no specific parameter
  ##  list(
  ##    shared = c(tau = 0.7, r = 0.1, sigma = 0.5, K = 1, X.0 = 11),
  ##    specific = array(
  ##      data = numeric(0),
  ##      dim = c(0, length(pPomp.object)),
  ##      dimnames = list(NULL,
  ##                      names(unitobjects(pPomp.object)))
  ##    )
  ##  )
  ##
  ###  # pstart with no shared parameter
  ##  list(shared = numeric(0),
  ##       specific = array(
  ##         data = c(11, 1, 0.71, 0.1, 0.5,
  ##                  22, 2, 0.72, 0.2, 0.55),
  ##         dim = c(5, length(pPomp.object)),
  ##         dimnames = list(c("X.0", "K", "tau", "r", "sigma"),
  ##                         names(unitobjects(pPomp.object)))
  ##         )
  ##       )
  #Np <- 50
  #prw.sd <- substitute(pomp::rw.sd(tau = 0.02, X.0 = ivp(0.2)))
  #transform <- TRUE
  #cooling.type <- "geometric"
  #cooling.fraction.50 <- 0.5
  #.ndone <- 0L
  ##  # END DEBUG
  
  # PRELIMS & BASIC CHECKS
  U <- as.integer(length(pPomp.object))
  Nmif <- as.integer(Nmif)
  # Check prw.sd: if it is not a list of 'rw.sd expressions,' make it one
  if (!is.list(prw.sd)) {
    prw.sd <- list(prw.sd)
    for (i.u in 1:U) {prw.sd[[i.u]] <- prw.sd[[1]]}
  }
  names.of.shared.parameters <- names(pstart[["shared"]])
  names.of.specific.parameters <- rownames(pstart[["specific"]])
  
  ########################################################
  # Initialize objects
  ########################################################
  
  # Initialize pParamMatrix
  pParamMatrix <- array(
    data = pstart[["shared"]],
    dim = c(length(names.of.shared.parameters), Np),
    dimnames = list(variable = names.of.shared.parameters,
                    rep = NULL)
  )
  # Initialize pparamArray
  pparamArray <- array(
    data = apply(pstart[["specific"]], 2, rep, times = Np),
    dim = c(length(names.of.specific.parameters), Np, U),
    dimnames = list(
      variable = names.of.specific.parameters,
      rep = NULL,
      unit = names(unitobjects(pPomp.object))
    )
  )
  # Initialize pconv.rec and pconv.rec.array
  completed.mif.iterations <- 0
  pconv.rec <- array(
    data = numeric(0),
    dim = c(Nmif + 1, length(pstart[["shared"]]) + 2),
    dimnames = list(
      iteration = seq.int(.ndone, .ndone + Nmif),
      variable = c('loglik', 'nfail', names(pstart[["shared"]]))
    )
  )
  pconv.rec[1L,-c(1:2)] <- pstart[["shared"]]
  
  pconv.rec.array <- array(
    data = numeric(0),
    dim = c(Nmif + 1, dim(pstart[["specific"]])[1] + 3, U),
    dimnames = list(
      iteration = seq.int(.ndone, .ndone + Nmif),
      variable = c('loglik', 'unitNfail', 'unitLoglik', dimnames(pstart[["specific"]])[[1]]),
      unit = names(unitobjects(pPomp.object))
    )
  )
  pconv.rec.array[1L, -c(1:3), ] <- pparamArray[, 1 ,]
  # Initialize output
  output <- list()
  # Create indices mapping loop indices 1:(Nmif*U) to unit indices 1:U
  unit.indices.for.entire.loop <- c(rep(1:U, times = Nmif))
  
  ###########################################################
  # LOOP OVER MIF ITERATIONS AND PANEL UNITS
  ###########################################################
  
for (i.loop.over.units.and.mif.iterations in 1:length(unit.indices.for.entire.loop)) {
    ## DEBUG
    # i.loop.over.units.and.mif.iterations <- as.integer(1)
    # i.loop.over.units.and.mif.iterations <- as.integer(2)
    # i.loop.over.units.and.mif.iterations <- as.integer(3)
    # i.loop.over.units.and.mif.iterations <- as.integer(4)
  
    # Find unit indices used in this loop round
    unit.index.for.current.loop.round <-
      unit.indices.for.entire.loop[i.loop.over.units.and.mif.iterations]
    
    # Create a (n updated) paramMatrix to pass to mif2 on next panel unit
    updated.paramMatrix <- rbind(pParamMatrix,
                                 pparamArray[, , unit.index.for.current.loop.round])
    # Here, we want to drop the unit dimension but, if there was only one specific parameter,
    # R will then have dropped its name, which we fix by
    rownames(updated.paramMatrix) <-
      c(rownames(pParamMatrix), rownames(pparamArray))
    
    # use pomp::mif2 the first time and pomp::continue after that
    if (i.loop.over.units.and.mif.iterations <= U) {
      output[[unit.index.for.current.loop.round]] <- try(pomp::mif2(
        object = pPomp.object@unit.objects[[unit.index.for.current.loop.round]],
        Np = Np,
        Nmif = 1,
        cooling.type = cooling.type,
        cooling.fraction.50 = cooling.fraction.50,
        transform = transform,
        rw.sd = eval(prw.sd[[unit.index.for.current.loop.round]]),
        #pomp:::pkern.sd(
        #  rw.sd = eval(prw.sd[[unit.index.for.current.loop.round]]),
        #  time = time(unitobjects(pPomp.object)[[1]]),
        #  paramnames = names(coef(unitobjects(pPomp.object)[[1]])),
        #  enclos = parent.frame()
        #),
        .paramMatrix = updated.paramMatrix,
        .indices = seq.int(Np)
      ))
    } else {
      # ... and run iterated filtering by continuing mif2d.pomp objects)
      output[[unit.index.for.current.loop.round]] <-
        try(pomp::continue(object = output[[unit.index.for.current.loop.round]],
                           .paramMatrix = updated.paramMatrix,
                           .indices = seq.int(Np)))
    }
    
    # Update (panelPomp) pParamMatrix with (pomp) paramMatrix ...
    pParamMatrix <-
      output[[unit.index.for.current.loop.round]]@paramMatrix[names.of.shared.parameters,
                                                              ,
                                                              drop = FALSE]
    # ... and update pparamArray:
    # first for the current unit ...
    pparamArray[names.of.specific.parameters,
                ,
                unit.index.for.current.loop.round] <-
      output[[unit.index.for.current.loop.round]]@paramMatrix[names.of.specific.parameters,
                                                              ,
                                                              drop = FALSE]
    # ... then, resample all other units using the mif2s.pomp indices
    pparamArray[names.of.specific.parameters,
                ,-unit.index.for.current.loop.round] <-
      pparamArray[names.of.specific.parameters,
                  output[[unit.index.for.current.loop.round]]@indices,
                  -unit.index.for.current.loop.round,
                  drop = FALSE]
    
    # Cleaning up: the default behaviour is to remove the paramMatrix
    # slot from the mif2d.pomp object to minimize memory requirements
    output[[unit.index.for.current.loop.round]]@paramMatrix <-
      array(data = numeric(0),
            dim = c(0, 0))
    # Finish by passing the updates onto pconv.rec and pconv.rec.array when appropriate
    if (unit.index.for.current.loop.round == U) {
      completed.mif.iterations <- completed.mif.iterations + 1
      # ... pconv.rec ...
      pconv.rec[completed.mif.iterations + 1, -c(1:2)] <-
        apply(X = pParamMatrix, MARGIN = 1, FUN = mean)
      pconv.rec[completed.mif.iterations + 1, "loglik"] <-
        sum(sapply(X = output, FUN = logLik))
      pconv.rec[completed.mif.iterations + 1, "nfail"] <-
        sum(sapply(
          X = output,
          FUN = function(x)
            x@nfail
        ))
      # ... and pconv.rec.array
      if (!is.null(names.of.specific.parameters)) {
        pconv.rec.array[completed.mif.iterations + 1,-c(1:3),] <-
          apply(
            X = pparamArray,
            MARGIN = 3,
            FUN = function(x)
              apply(
                X = x,
                MARGIN = 1,
                FUN = mean
              )
          )
        pconv.rec.array[completed.mif.iterations + 1, "loglik", ] <-
          sum(sapply(X = output, FUN = logLik))
        pconv.rec.array[completed.mif.iterations + 1, "unitLoglik", ] <-
          sapply(X = output, FUN = logLik)
        pconv.rec.array[completed.mif.iterations + 1, "unitNfail",] <-
          sapply(
            X = output,
            FUN = function(x) {
              x@nfail
            }
          )
      }
    }
  }
  #######################################################
  # BEFORE RETURNING OUTPUT, PERFORM FINAL ARRANGEMENTS #
  #######################################################
  
  # Extract loglikelihoods
  unit.logliks <- sapply(X = output, FUN = logLik)
  ploglik <- sum(unit.logliks)
  # name  output
  names(output) <- names(unitobjects(pPomp.object))
  # create pParams slot from last mif iteration values in pconv.rec
  pParams <- list(shared = NA, specific = NA)
  pParams[["shared"]] <- pconv.rec[nrow(pconv.rec),-c(1:2), drop = FALSE][1, ]
  # Here, we want to drop the iteration dimension but, if there was only one shared 
  # parameter, R will then have dropped its name, which we fix by
  names(pParams[["shared"]]) <- dimnames(pconv.rec)$variable[-c(1:2)]
  pParams[["specific"]] <- 
    aperm(
      a = pconv.rec.array[nrow(pconv.rec),-c(1:3), , drop = FALSE],
      perm = c(2, 3, 1)
      )[, , 1]
  # if there is only one specific parameter, R will "overdrop" and return a vector
  # instead of the matrix resulting from dropping the iteration dimension
  if (length(names.of.specific.parameters)==1) {
    pParams[["specific"]] <- matrix(
      data = pParams[["specific"]],
      nrow = 1,
      dimnames = list(
        variable = colnames(pconv.rec.array)[-c(1:3)],
        unit = dimnames(pconv.rec.array)$unit        
        )
      )
  }
  ptol <-
    structure(
      .Data = sapply(output, function(mif2d.pomp)
        mif2d.pomp@tol),
      names = names(unitobjects(object = pPomp.object))
    )
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