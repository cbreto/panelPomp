if (file.exists("_options.R")) source("_options.R")
library(panelPomp,quietly=TRUE)

TESTS_PASS <- NULL
test <- function(expr1,expr2,all="TESTS_PASS",env=parent.frame(),...)
  panelPomp:::test(expr1,expr2,all=all,env=env,...)

pg <- panelGompertz(U=3,N=4)
gompertz <- as(pg,"list")[[1]]
coef(gompertz) <- c(pparams(pg)$sh, pparams(pg)$sp[,1])

shgomp <- gompertz
time(shgomp) <- time(gompertz)[1:2]
shgomp@data <- gompertz@data[,1:length(time(shgomp)),drop=FALSE]
short.pgompertz <- panelPomp(list(u1=shgomp,u2=shgomp),shared=coef(gompertz))
pPomp.object <- short.pgompertz
mf <- mif2(pPomp.object,Np=10,cooling.type="geometric",cooling.fraction.50=0.5,
           rw.sd=rw_sd(tau=0.02,X.0=ivp(0.2)))
## only one specific parameter
test("mif2d.ppomp",class(panelPomp:::mif2.internal(
  pPomp.object,Nmif=2,start=list(
    shared=c(K=1.0,r=0.1,sigma=0.1,tau=0.1),
    specific=array(1,dim=c(1,length(pPomp.object)),
                   dimnames=list(c("X.0"),names(unitobjects(pPomp.object))))),
  Np=50,rw.sd=rw_sd(tau=0.02,X.0=ivp(0.2)),cooling.type="geometric",
  cooling.fraction.50=0.5,block=FALSE))[1])
## both shared and specific parameters
test("mif2d.ppomp",class(panelPomp:::mif2.internal(
  pPomp.object,Nmif=2,start=list(
    shared=c(r=0.1,sigma=0.1,tau=0.1),
    specific=array(c(1,1),dim=c(2,length(pPomp.object)),
                   dimnames=list(c("X.0","K"),names(unitobjects(pPomp.object))))),
  Np=50,rw.sd=rw_sd(tau=0.02,X.0=ivp(0.2)),cooling.type="geometric",
  cooling.fraction.50=0.5,block=FALSE))[1])
## only one shared parameter
test("mif2d.ppomp",class(panelPomp:::mif2.internal(
  pPomp.object,Nmif=2,start=list(
    shared=c(tau=0.1),
    specific=array(c(1,1,0.1,0.1),dim=c(4,length(pPomp.object)),
                   dimnames=list(c("X.0","K","r","sigma"),
                                 names(unitobjects(pPomp.object))))),
  Np=50,rw.sd=rw_sd(tau=0.02,X.0=ivp(0.2)),cooling.type="geometric",
  cooling.fraction.50=0.5,block=FALSE))[1])
## mif2d.ppomps can be mif2d again
test("mif2d.ppomp",class(mif2(mf))[1])
## mif2.internal pomp::mif2::tryCatch works
test(class(try(mif2(pg,Nmif=2,Np=50,rw.sd=rw_sd(something=0.02,random=ivp(0.2)),
                    cooling.type="geometric",cooling.fraction.50=0.5),
               silent=TRUE))[1],
     "try-error")

## check whether all tests passed
all(get(eval(formals(test))$all))
if (!all(get(eval(formals(test))$all))) stop("Not all tests passed!")
