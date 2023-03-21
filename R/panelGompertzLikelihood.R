##' Likelihood for a panel Gompertz model via a Kalman filter
##'
##' Evaluates the likelihood function for a panel Gompertz model,
##' using a format convenient for maximization by optim() to obtain a maximum
##' likelihood estimate. Specifically, estimated and fixed parameters are
##' supplied by two different arguments.
##'
##' @param x named vector for a subset of parameters, corresponding to
##' those being estimated.
##'
##' @param panelPompObject a panel Gompertz model.
##'
##' @param params named vector containing all the parameters of the panel Gompertz model. Estimated parameters are overwritten by x.
##'
##' @returns
##' A \code{numeric} value.
##'
##' @author Edward L. Ionides
##'
##' @examples
##' pg <- panelGompertz(N=2,U=2)
##' panelGompertzLikelihood(coef(pg),pg,coef(pg))
##' @export
panelGompertzLikelihood <- function(x,panelPompObject,params){
  gompertzLikelihood <- function (pompObject) {
    theta <- coef(pompObject)
    pompObject@data <- log(obs(pompObject))
    X0 <- c(logX=log(1))
    A <- matrix(exp(-theta["r"]),1,1)
    Q <- matrix(theta["sigma"]^2,1,1)
    C <- matrix(1,1,1)
    R <- matrix(theta["tau"]^2,1,1)
    pomp::kalmanFilter(pompObject, X0, A, Q, C, R)$logLik - sum(obs(pompObject))
  }
  p <- params
  p[names(x)] <- x
  coef(panelPompObject) <- p
  sum(sapply(as(panelPompObject,"list"),gompertzLikelihood))
}
