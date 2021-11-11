#' @include panelGompertz.R
NULL

#' Gompertz Kalman filter.
#'
#' Gompertz Kalman filter.
#'
#' @param Y data.
#' @param X0 initial value for state variable X.
#' @param r r parameter.
#' @param K K parameter.
#' @param sigma sigma parameter.
#' @param tau tau parameter.
#' @param from_est logical; whether to partrans with from_est before the final calculation (e.g., if passing to optim as opposed to evaluating directly).
#' @param po pomp; Gompertz object to use with partrans (if transform is TRUE).
#'
#'
kalman_gompertz <- function (Y,X0,r,K,sigma,tau,from_est=FALSE,po=NULL) {
  if (from_est) {
    if (!"from.trans"%in%slotNames(po)) {
      stop(paste0("in ",sQuote("kalman_gompertz"),": "),sQuote("from_est"),
           " set to TRUE but there is no from.trans slot in (pomp) obj",call.=FALSE)
    }
    p <- c(X.0=X0,r=r,K=K,sigma=sigma,tau=tau)
    p <- partrans(object=po,params=p,dir="fromEstimationScale")
    X0 <- p["X.0"]
    r <- p["r"]
    K <- p["K"]
    sigma <- p["sigma"]
    tau <- p["tau"]
  }
  ntimes <- length(Y)
  sigma.sq <- sigma^2
  tau.sq <- tau^2
  cond.loglik <- numeric(ntimes)
  filter.mean <- numeric(ntimes)
  pred.mean <- numeric(ntimes)
  pred.var <- numeric(ntimes)
  m <- log(X0)
  v <- 0
  S <- exp(-r)
  for (k in seq_len(ntimes)) {
    pred.mean[k] <- M <- (1-S)*log(K) + S*m
    pred.var[k] <- V <- S*v*S+sigma.sq
    q <- V+tau.sq
    r <- log(Y[k])-M
    cond.loglik[k] <- dnorm(x=log(Y[k]),mean=M,sd=sqrt(q),log=TRUE)-log(Y[k])
    q <- 1/V+1/tau.sq
    filter.mean[k] <- m <- (log(Y[k])/tau.sq+M/V)/q
    v <- 1/q
  }
  list(
    pred.mean=pred.mean,
    pred.var=pred.var,
    filter.mean=filter.mean,
    cond.loglik=cond.loglik,
    loglik=sum(cond.loglik)
  )
}

#' Kalman filter for a Gompertz pomp object.
#'
#' Kalman filter for a Gompertz pomp object.
#'
#' @param x parameter values over which to optimize.
#' @param params numeric vector with values for parameter not specified in x.
#' @param xnm Including the names of x in xnm allows reassigning them if dropped by optim (as happens with the Brent method for in univariate optimization).
#' @inheritParams kalman_gompertz
#'
#'
kalman_gompertz_u <- function (x,po,params,xnm=NULL,from_est=FALSE) {
  Y <- obs(po)
  p <- params
  namesx <- if (is.null(names(x))) xnm else names(x)
  p[namesx] <- x
  X0 <- unname(p["X.0"])
  r <- unname(p["r"])
  K <- unname(p["K"])
  sigma <- unname(p["sigma"])
  tau <- unname(p["tau"])
  -kalman_gompertz(Y=Y,X0=X0,r=r,K=K,sigma=sigma,tau=tau,
                 from_est=from_est,po=po)$loglik
}

#' Evaluates the Kalman filter loglikelihood of a panel Gompertz.
#'
#' Evaluates the Kalman filter loglikelihood of a panel Gompertz.
#'
#' @param sh shared parameters values.
#' @param obj panelPomp object.
#' @param pParams list of shared and specific parameters.
#' @param unit_fn function to evaluate the unit loglikelihood.
#' @param shnm shared parameter names; needed because, if using optim's Brent (as recommended by optim when there is only one shared parameter), optim strips these off the sh argument.
#' @param allout logical; whether to return all the optimization output (TRUE) or only the optimized values (FALSE, as needed when passed to \code{optim}).
#' @inheritParams kalman_gompertz
#'
#'
kalman_panel <- function (sh,obj,pParams,unit_fn,shnm=NULL,allout=FALSE,from_est=FALSE) {
  if (!is.null(shnm) & !is.null(names(sh))) stopifnot(identical(names(sh),shnm))
  names.sh <- if (is.null(names(sh))) shnm else names(sh)
  ll <- list()
  pParams$sh[names.sh] <- sh
  for (i.u in seq_len(length(obj))) {
    p <- c(pParams$sh,get_col(pParams$sp,,i.u))
    ll[[i.u]] <- unit_fn(x=NULL,po=as(obj,"list")[[i.u]],params=p,from_est=from_est)
  }
  if (allout==TRUE) {
    list(ll=sum(unlist(ll)),lls=setNames(ll,nm=names(obj)))
  } else {
    sum(unlist(ll))
  }
}
