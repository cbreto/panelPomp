#' @include panelPomp-package.R
NULL

## change the "pomp.examples' global option when the "panelPomp" package is loaded/unloaded
.onAttach <- function (...) {
  exampleDir <- getOption("pomp.examples")
  newDir <- system.file("examples",package="panelPomp")
  options(pomp.examples=c(exampleDir,newDir,recursive=TRUE))
}

.onDetach <- function (...) {
  exampleDir <- getOption("pomp.examples")
  newDir <- system.file("examples",package="panelPomp")
  exampleDir <- exampleDir[exampleDir!=newDir]
  options(pomp.examples=exampleDir)
}

## Go to list-form pparams from matrix specification
toListPparams <- function(
  matrixPparams,
  names.in.vector,
  vector.position.in.listPparams,
  vector.name.in.listPparams,
  matrix.name.in.listPparams
  ) {
  output <- as.vector(c(NA,NA), mode = "list")
  # fill vector in
  output[[vector.position.in.listPparams]] <- matrixPparams[names.in.vector,1]
  # fill matrix in
  name.logicals.for.matrix <- !rownames(matrixPparams) %in% names.in.vector
  output[[ifelse(vector.position.in.listPparams==1,2,1)]] <-
    if(any(name.logicals.for.matrix)) {
      matrixPparams[name.logicals.for.matrix, , drop = FALSE]} else {
        # fill empty matrix in
        array(
          data = numeric(0), 
          dim = c(0, dim(matrixPparams)[2]),
          dimnames = list(NULL, dimnames(matrixPparams)[[2]])
          )
      }
  names(output)[vector.position.in.listPparams] <- vector.name.in.listPparams
  names(output)[ifelse(vector.position.in.listPparams==1,2,1)] <- matrix.name.in.listPparams
  output
  }


## Go to matrix-form pparams from list specification
toMatrixPparams <- function(listPparams){
  # DEBUG=T
  #if(DEBUG==T){
  #  common <- c(common.1 = 1, common.2 = 2)
  #  u <- 5 # at least 1
  #  specific <- matrix(
  #    as.numeric(paste0(rep(seq(3,u+2),each=2),rep(c(".1",".2"),times=u))),
  #    nrow = 2, dimnames = list(c("spec.1", "spec.2") , c(paste0("unit.",1:u))))
  #  #
  #  #listPparams <- listPparams.common.only <- list(common = common, specific = do.call(cbind, sapply(1:u, as.matrix, x=numeric(0))))
  #  #listPparams <- listPparams.specific.only <- list(common = numeric(0), specific = specific)
  #  listPparams <- listPparams.mixec <- list(common = common, specific = specific)
  #}
  common.params <- if(any(sapply(listPparams,is.vector))) {
     listPparams[[which(!sapply(listPparams,is.matrix))]]} else {NULL}
  specific.params <- if(any(unname(unlist(lapply(listPparams,is.matrix))))) {
     listPparams[[which(sapply(listPparams,is.matrix))]]} else {NULL}

  U <- dim(specific.params)[2]
  matrixPparams <- rbind(if (is.null(common.params)) {
                             NULL
                         } else {
                             matrix(
                                 rep(x = common.params, times = U),
                                 ncol = U,
                                 dimnames = list(names(common.params), NULL)
                             )
                         },
                         specific.params)
    matrixPparams
  #if(DEBUG==T) print(x = list(listPparams = listPparams))
}

## Uniform random draws in the transformed scale: give centers and widths
runif.EstimationScale <-
  function(centers, widths,
           toEstimationScale.fn = log, fromEstimationScale.fn = exp) {
    transformed.centers <- toEstimationScale.fn(centers)
    fromEstimationScale.fn(runif(
      n = length(centers), min = transformed.centers - widths*0.5,
      max = transformed.centers + widths*0.5
    ))
  }# END FN runif.EstimationScale


## kalman filter internal functions
#gompertz.kalman.filter <- function(Y, X0, r, K, sigma, tau) {
#  ntimes <- length(Y)
#  sigma.sq <- sigma ^ 2
#  tau.sq <- tau ^ 2
#  cond.loglik <- numeric(ntimes)
#  filter.mean <- numeric(ntimes)
#  pred.mean <- numeric(ntimes)
#  pred.var <- numeric(ntimes)
#  m <- log(X0)
#  v <- 0
#  S <- exp(-r)
#  for (k in seq_len(ntimes)) {
#    pred.mean[k] <- M <- (1 - S) * log(K) + S * m
#    pred.var[k] <- V <- S * v * S + sigma.sq
#    q <- V + tau.sq
#    r <- log(Y[k]) - M
#    cond.loglik[k] <-
#      dnorm(
#        x = log(Y[k]),mean = M,sd = sqrt(q),log = TRUE
#      ) - log(Y[k])
#    q <- 1 / V + 1 / tau.sq
#    filter.mean[k] <- m <- (log(Y[k]) / tau.sq + M / V) / q
#    v <- 1 / q
#  }
#  list(
#    pred.mean = pred.mean,
#    pred.var = pred.var,
#    filter.mean = filter.mean,
#    cond.loglik = cond.loglik,
#    loglik = sum(cond.loglik)
#  )
#}

#negative.gompertz.Kalman.loglik <- function(x, object, params) {
#  # evaluates gompertz likelihood
#  # parameters X0 and K are fixed at 1
#  # other parameters are taken from x (or, if not in x, from params)
#  Y <- obs(object)
#  p <- params
#  p[names(x)] <- x
#  X0 <- 1
#  r <- p["r"]
#  K <- 1
#  sigma <- p["sigma"]
#  tau <- p["tau"]
#  - gompertz.kalman.filter(Y, X0, r, K, sigma, tau)$loglik
#}


## This version works with common parameters
#negative.gompertz.panel.Kalman.loglik <-
#  function(x, object, params) {
#    # evaluates gompertz likelihood for a 2-unit panel
#    # parameters X0 and K are fixed at 1
#    # with common parameters taken from x (or, if not in x, from params)
#    Y1 <- obs(object@unit.objects[[1]])
#    Y2 <- obs(object@unit.objects[[2]])
#    p <- params
#    p[names(x)] <- x
#    X0 <- 1
#    r <- p["r"]
#    K <- 1
#    sigma <- p["sigma"]
#    tau <- p["tau"]
#    - (gompertz.kalman.filter(Y1, X0, r, K, sigma, tau)$loglik + gompertz.kalman.filter(Y2, X0, r, K, sigma, tau)$loglik)
#  }