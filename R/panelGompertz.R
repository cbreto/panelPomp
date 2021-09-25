##' Panel Gompertz model
##'
##' Given a collection of points maximizing the likelihood over a range
##' of fixed values of a focal parameter, this function constructs 
##' a profile likelihood confidence interval accommodating both
##' Monte Carlo error in the profile and statistical uncertainty present
##' in the likelihood function.
##' 
##' @param N number of observations for each unit.
##'
##' @param U number of units.
##' 
##' @param params parameter vector, assuming all units have the same parameters.
##'
##' @param seed passed to the random number generator for simulation.
##'
##' @author Edward L. Ionides
##'
##' @export
##'
panelGompertz <- function(N=100,U=50,
  params=c(K=1,r=0.1,sigma=0.1,tau=0.1,X.0=1),
  seed=12345678){

  gomp_dmeas <- Csnippet("lik = dlnorm(Y,log(X),tau,give_log);")
  gomp_rmeas <- Csnippet("Y = rlnorm(log(X),tau);") 
  gomp_rproc <- Csnippet("
    double S = exp(-r*dt);
    double eps = (sigma > 0.0) ? rlnorm(0,sigma) : 1.0;
    X = pow(K,(1-S))*pow(X,S)*eps;")

  # check for existing 'cdir' (to make 'testthat' package work)
  # also, maybe this is needed for Windows compatibility?
  cdir <- if (exists("cdir",inherits=FALSE)) cdir else NULL

  gomp_pomp <- pomp(data=data.frame(t=1:N,Y=NA),
    times="t",
    t0=0,
    rprocess=discrete_time(step.fun=gomp_rproc,delta.t=1),
    rmeasure=gomp_rmeas,
    dmeasure=gomp_dmeas,
    params=params,
    paramnames=c("K","r","sigma","tau","X.0"),
    partrans=parameter_trans(log=c("K","r","sigma","tau","X.0")),
    statenames=c("X"),
    cdir=cdir)
  gomp_sim_list <- setNames(vector(mode="list",length=U),nm=paste0("unit",1:U))
  for (u in seq_len(U)) {
    gomp_sim_list[[u]] <- pomp::simulate(gomp_pomp,seed=seed+u)
  }  
  setNames(gomp_sim_list,nm=paste0("unit",1:U))
  gomp_shared_names <- c("r","sigma")
  gomp_specific <- coef(gomp_pomp)[!names(coef(gomp_pomp))%in%gomp_shared_names]
  panelPomp(gomp_sim_list,
    shared=coef(gomp_pomp)[gomp_shared_names],
    specific= matrix(gomp_specific,
      nrow=length(gomp_specific),
      ncol=U,
      dimnames=list(names(gomp_specific),names(gomp_sim_list)))
  ) 
}

