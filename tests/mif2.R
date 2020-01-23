library(panelPomp,quietly=TRUE)
if (file.exists("options.R")) source("options.R")

TESTS_PASS <- NULL
test <- function(expr1,expr2,all="TESTS_PASS",env=parent.frame(),...) 
  panelPomp:::test(expr1,expr2,all=all,env=env,...)

ppo <- panelPomp:::pompExample(prw,envir=NULL)[[1]]
po <- ppo[[1]]

ep <- wQuotes("Error : in ''mif2'': ")
et <- wQuotes(" (''mif2,panelPomp-method'')\n")
## test checks for missing arguments
test(wQuotes(ep,"''data'' is a required argument.\n"),
     mif2(Np=10,rw.sd=rw.sd(sigmaX=0.05,X.0=0.5),cooling.fraction.50=0.5))
test(wQuotes(ep,"Missing ''Np'' argument.",et),
     mif2(ppo,rw.sd=rw.sd(sigmaX=0.05,X.0=0.5),cooling.fraction.50=0.5)) 
test(wQuotes(ep,"missing ''rw.sd'' argument.",et),
     mif2(ppo,Np=10,cooling.fraction.50=0.5))
test(wQuotes(ep,"Missing ''cooling.fraction.50'' argument.",et),
     mif2(ppo,Np=10,rw.sd=rw.sd(sigmaX=0.05,X.0=0.5)))
test(wQuotes(ep,"pomp's ''mif2'' error message: in ''mif2'': the following ",
             "parameter(s), given random walks in ''rw.sd'', are not present ",
             "in ''params'': ''X.0''. (panelPomp:::mif2.internal)\n"),
     mif2(panelPomp(unitobjects(ppo)),Np=10,rw.sd=rw.sd(sigmaX=0.05,X.0=0.5),
         cooling.fraction.50=0.5,sh=pparams(ppo)$sh))
test(wQuotes(ep,"pomp's ''mif2'' error message: in ''mif2'': 'arg' must be ",
             "of length 1 (panelPomp:::mif2.internal)\n"),
     mif2(panelPomp(unitobjects(ppo),shared=coef(po)),Np=10,sp=pparams(ppo)$sp,
          rw.sd=rw.sd(sigmaX=0.05,X.0=0.5),cooling.fraction.50=0.5))
## assign parameters
test(# no start (get from object)
  traces(as(mif2(ppo,Np=10,rw.sd=rw.sd(X.0=0.2),cooling.fraction.50=0.5,
                 cooling.type="geometric"),"list")[[1]])[1,-1],
  c(ppo@shared,get_col(ppo@specific,1,1)))
test(# start shared & specific
  traces(as(mif2(ppo,sh=2*ppo@shared,sp=2*ppo@specific,Np=10,
                 rw.sd=rw.sd(X.0=0.2),cooling.fraction.50=0.5,
                 cooling.type="geometric"),"list")[[1]])[1,-1],
  2*c(ppo@shared,get_col(ppo@specific,1,1)))
test(# start shared only
  traces(as(mif2(ppo,shared.start=2*ppo@shared,Np=10,rw.sd=rw.sd(X.0=0.2),
                 cooling.fraction.50=0.5,cooling.type="geometric"),
            "list")[[1]])[1,-1],
  c(2*ppo@shared,get_col(ppo@specific,1,1)))
test(# start specific only
  traces(as(mif2(ppo,sp=2*ppo@specific,Np=10,rw.sd=rw.sd(X.0=0.2),
                 cooling.fraction.50=0.5,cooling.type="geometric"),
            "list")[[1]])[1,-1],
  c(ppo@shared,2*get_col(ppo@specific,1,1)))
test(# start with list
  traces(as(mif2(ppo,st=list(shared=2*ppo@shared,specific=2*ppo@specific),
                 Np=10,rw.sd=rw.sd(X.0=0.2),cooling.fraction.50=0.5,
                 cooling.type="geometric"),"list")[[1]])[1,-1],
  2*c(ppo@shared,get_col(ppo@specific,1,1)))
test(# start with numeric vector
  traces(as(mif2(
    ppo,
    st=setNames(c(ppo@shared,ppo@specific),
                c(names(ppo@shared),"X.0[rw1]","X.0[rw2]")),
    Np=10,rw.sd=rw.sd(X.0=0.2),
    cooling.fraction.50=0.5,
    cooling.type="geometric"),"list")[[1]])[1,-1],
  c(ppo@shared,get_col(ppo@specific,1,1)))
## resolve multiple params
test(wQuotes(ep,"specify EITHER ''start'' only OR ''shared.start'' and/or",
             " ''specific.start''. (''mif2,panelPomp-method'')\n"),
     mif2(ppo,shared=2*ppo@shared,
          start=list(specific=ppo@specific,shared=ppo@shared),Np=10,
          rw.sd=rw.sd(X.0=0.2),cooling.fraction.50=0.5,
          cooling.type="geometric"))
test(wQuotes(ep,"specify EITHER ''start'' only OR ''shared.start'' and/or",
             " ''specific.start''. (''mif2,panelPomp-method'')\n"),
     mif2(ppo,specific=2*ppo@specific,
          start=list(shared=ppo@shared,specific=ppo@specific),Np=10,
          rw.sd=rw.sd(X.0=0.2),cooling.fraction.50=0.5,
          cooling.type="geometric"))
test(wQuotes(ep,"specify EITHER ''start'' only OR ''shared.start'' and/or",
             " ''specific.start''. (''mif2,panelPomp-method'')\n"),
     mif2(ppo,sh=2*ppo@shared,sp=2*ppo@specific,
          st=list(shared=ppo@shared,specific=ppo@specific),
          Np=10,rw.sd=rw.sd(sigmaX=0.05,X.0=0.5),cooling.fraction.50=0.5,
          cooling.type="geometric"))
## wrong unit names
test(wQuotes(ep,"specific parameter column-names must match the names of the ",
             "units\n"),
     {sp <- ppo@specific
     colnames(sp) <- paste0(colnames(sp), "_")
     mif2(ppo,Np=10,sp=sp,rw.sd=rw.sd(X.0=0.2),cooling.fraction.50=0.5,
          cooling.type="geometric")})
## wrong unit-specific names
test(wQuotes(ep,"pomp's ''mif2'' error message: in ''mif2'': the following ",
             "parameter(s), given random walks in ''rw.sd'', are not present ",
             "in ''params'': ''X.0''. (panelPomp:::mif2.internal)\n"),
     {sp <- ppo@specific
     rownames(sp) <- c("some_wrong_name")
     mif2(ppo,Np=10,sp=sp,rw.sd=rw.sd(X.0=0.2),cooling.fraction.50=0.5,
          cooling.type="geometric")})
##  wrong shared names
test(wQuotes(ep,"pomp's ''mif2'' error message: in ''mif2'': in ''rprocess'': ",
             "variable 'sigmaY' not found among the parameters. ",
             "(panelPomp:::mif2.internal)\n"),
     mif2(ppo,Np=10,sh=c(sth=0),rw.sd=rw.sd(X.0=0.2),cooling.fraction.50=0.5,
          cooling.type="geometric"))

mf <- mif2(ppo,Np=10,rw.sd=rw.sd(X.0=0.2),
           cooling.fraction.50=0.5,cooling.type="geometric")
mf <- mif2(mf,Nmif=2,start=coef(mf))

wQuotes(ep,"specify EITHER ''start'' only OR ''shared.start'' and/or",
        " ''specific.start''. (''mif2,mif2d.ppomp-method'')\n") -> err
test(err,mif2(mf,Nmif=2,start=coef(mf),sh=2*ppo@shared,sp=2*ppo@specific))
test(err,mif2(mf,Nmif=2,start=coef(mf),sp=2*ppo@specific))
test(err,mif2(mf,Nmif=2,start=coef(mf),sh=2*ppo@shared))

test(dim(traces(mf)),c(3L,7L))
test(dim(traces(mf,c("loglik","sigmaY"))),c(3L,2L))
test(dim(traces(mf,c("loglik","sigmaY","X.0"))),c(3L,4L))
test(dim(traces(mf,c("loglik","unitLoglik"))),c(3L,3L))


## check whether all tests passed
all(get(eval(formals(test))$all))
if (!all(get(eval(formals(test))$all))) stop("Not all tests passed!")
