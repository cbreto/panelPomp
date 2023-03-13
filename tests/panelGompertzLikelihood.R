if (file.exists("_options.R")) source("_options.R")
library(panelPomp,quietly=TRUE)

TESTS_PASS <- NULL
test <- function(expr1,expr2,all="TESTS_PASS",env=parent.frame(),...)
  panelPomp:::test(expr1,expr2,all=all,env=env,...)

g <- panelGompertz(N=5,U=2)
estNames<- c("r",paste0("tau[unit",1:length(g),"]"))
theta_start <- coef(g)[estNames]
gLik <- panelGompertzLikelihood(theta_start,g,coef(g))
gLik

stew(file="panelGompertzLikelihood.rda",seed=2358,{
  g_pfLik <- logLik(pfilter(g,Np=5000))
})
test(abs(g_pfLik-gLik)<1,TRUE)

gMLE <- optim(
    par=theta_start,
    fn=panelGompertzLikelihood,
    panelPompObject=g,
    params=coef(g),
    hessian=TRUE,
    control=list(trace=0,fnscale=-1)
  )

gMLE$val
gMLE$par
theta_start

## check whether all tests passed
all(get(eval(formals(test))$all))
if (!all(get(eval(formals(test))$all))) stop("Not all tests passed!")


