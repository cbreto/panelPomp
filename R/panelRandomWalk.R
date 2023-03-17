#' @include mif2_methods.R
NULL

##' Panel random walk model
##'
##' Builds a collection of independent realizations from a random
##' walk model.
##'
##' @param N number of observations for each unit.
##'
##' @param U number of units.
##'
##' @param params parameter vector, assuming all units have the same parameters.
##'
##' @param seed passed to the random number generator for simulation.
##'
##' @author Edward L. Ionides, Carles Breto
##'
##' @return
##' A \code{panelPomp} object.
##' @examples
##' panelRandomWalk()
##' @export
panelRandomWalk <- function(N=5,U=2,
  params=c(sigmaY=1,sigmaX=1,X.0=1),seed=3141592){
  times <- 1:N
  t0 <- 0
  rproc <- Csnippet("X = X + rnorm(0,sigmaX);")
  delta_t <- 1
  rw_rmeas <- Csnippet("Y = rnorm(X,sigmaY);")
  rw_dmeas <- Csnippet("lik = dnorm(Y,X,sigmaY,give_log);")
  rw_paramnames <- names(params)
  rw_statenames <- c("X")

  # check for existing 'cdir' (to make 'testthat' package work)
  # also, maybe this is needed for Windows compatibility?
  cdir <- if (exists("cdir",inherits=FALSE)) cdir else NULL

  rw_pomp <- pomp(data.frame(t=times,Y=NA),times="t",t0=t0,
     rprocess=discrete_time(step.fun=rproc,delta.t=delta_t),
     rmeasure=rw_rmeas,
     dmeasure=rw_dmeas,
     params=params,
     paramnames=rw_paramnames,
     statenames=rw_statenames,
     cdir=cdir)
  rw_sim_list <- setNames(vector("list",length=U),nm=paste0("rw",1:U))
  for (u in seq_len(U)) {
    rw_sim_list[[u]] <- pomp::simulate(rw_pomp,seed=seed+u)
  }
  setNames(rw_sim_list,nm=paste0("unit",1:U))

  rw_shared_names <- c("sigmaX","sigmaY")
  rw_specific <- coef(rw_pomp)[!names(coef(rw_pomp))%in%rw_shared_names]

  panelPomp(rw_sim_list,
    shared=coef(rw_pomp)[rw_shared_names],
    specific= matrix(rw_specific,
      nrow=length(rw_specific),
      ncol=U,
      dimnames=list(param=names(rw_specific),unit=names(rw_sim_list)))
  )
}


# test
#library(panelPomp)
#p <- panelRandomWalk()
#p2 <- panelRandomWalk(N=10,U=4,params=c(sigmaY=0.5,sigmaX=2,X.0=0),seed=123)
