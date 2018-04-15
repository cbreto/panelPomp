library(panelPomp,quietly=TRUE)

TESTS_PASS <- NULL
test <- function(expr1,expr2,all="TESTS_PASS",env=parent.frame(),...) 
  panelPomp:::test(expr1,expr2,all=all,env=env,...)

ppo <- pompExample(prw,envir=NULL)[[1]]
po <- ppo[[1]]

ep <- wQuotes("Error : in ''mif2'': ")
et <- wQuotes(" (''mif2,panelPomp-method'')\n")
## test checks for missing arguments
test(wQuotes(ep,"''object'' is a required argument\n"),
     mif2(Np=10,rw.sd=rw.sd(sigmaX=0.05,X.0=0.5),cooling.fraction.50=0.5))
test(wQuotes(ep,"Missing ''Np'' argument.",et),
     mif2(ppo,rw.sd=rw.sd(sigmaX=0.05,X.0=0.5),cooling.fraction.50=0.5)) 
test(wQuotes(ep,"missing ''rw.sd'' argument.",et),
     mif2(ppo,Np=10,cooling.fraction.50=0.5))
test(wQuotes(ep,"Missing ''cooling.fraction.50'' argument.",et),
     mif2(ppo,Np=10,rw.sd=rw.sd(sigmaX=0.05,X.0=0.5)))
try(mif2(panelPomp(unitobjects(ppo)),Np=10,rw.sd=rw.sd(sigmaX=0.05,X.0=0.5),
         cooling.fraction.50=0.5,sh=pparams(ppo)$sh))
try(mif2(panelPomp(unitobjects(ppo),shared=coef(po)),Np=10,sp=pparams(ppo)$sp,
          rw.sd=rw.sd(sigmaX=0.05,X.0=0.5),cooling.fraction.50=0.5))
## assign parameters
test(conv.rec(as(mif2(ppo,Np=10,rw.sd=rw.sd(X.0=0.2),cooling.fraction.50=0.5,
  cooling.type="geometric"),"list")[[1]])[1,-(1:2)],
  c(ppo@shared,get_col(ppo@specific,1,1)))
test(conv.rec(as(mif2(ppo,sh=2*ppo@shared,sp=2*ppo@specific,Np=10,
                      rw.sd=rw.sd(X.0=0.2),cooling.fraction.50=0.5,
                      cooling.type="geometric"),"list")[[1]])[1,-(1:2)],
     2*c(ppo@shared,get_col(ppo@specific,1,1)))
test(conv.rec(as(mif2(ppo,shared.start=2*ppo@shared,Np=10,rw.sd=rw.sd(X.0=0.2),
                      cooling.fraction.50=0.5,cooling.type="geometric"),
                 "list")[[1]])[1,-(1:2)],
     c(2*ppo@shared,get_col(ppo@specific,1,1)))
test(conv.rec(as(mif2(ppo,sp=2*ppo@specific,Np=10,rw.sd=rw.sd(X.0=0.2),
                      cooling.fraction.50=0.5,cooling.type="geometric"),
                 "list")[[1]])[1,-(1:2)],
     c(ppo@shared,2*get_col(ppo@specific,1,1)))
test(conv.rec(as(mif2(ppo,st=list(shared=2*ppo@shared,specific=2*ppo@specific),
  Np=10,rw.sd=rw.sd(X.0=0.2),cooling.fraction.50=0.5,
  cooling.type="geometric"),"list")[[1]])[1,-(1:2)],
  2*c(ppo@shared,get_col(ppo@specific,1,1)))
## resolve multiple params
try(mif2(ppo,shared=2*ppo@shared,
  start=list(specific=ppo@specific,shared=ppo@shared),Np=10,
  rw.sd=rw.sd(X.0=0.2),cooling.fraction.50=0.5,
  cooling.type="geometric"))
try(mif2(ppo,specific=2*ppo@specific,
  start=list(shared=ppo@shared,specific=ppo@specific),Np=10,
  rw.sd=rw.sd(X.0=0.2),cooling.fraction.50=0.5,
  cooling.type="geometric"))
try(mif2(ppo,sh=2*ppo@shared,sp=2*ppo@specific,
          st=list(shared=ppo@shared,specific=ppo@specific),
          Np=10,rw.sd=rw.sd(sigmaX=0.05,X.0=0.5),cooling.fraction.50=0.5,
          cooling.type="geometric"))
## wrong unit names
try({
  sp <- ppo@specific
  colnames(sp) <- paste0(colnames(sp), "_")
  mif2(ppo,Np=10,sp=sp,rw.sd=rw.sd(X.0=0.2),cooling.fraction.50=0.5,
       cooling.type="geometric")
})
## wrong unit-specific names
try({
  sp <- ppo@specific
  rownames(sp) <- c("some_wrong_name")
  mif2(ppo,Np=10,sp=sp,rw.sd=rw.sd(X.0=0.2),cooling.fraction.50=0.5,
       cooling.type="geometric")
  })
##  wrong shared names
try(mif2(ppo,Np=10,sh=c(sth=0),rw.sd=rw.sd(X.0=0.2),cooling.fraction.50=0.5,
          cooling.type="geometric"))

mf <- mif2(ppo,Np=10,rw.sd=rw.sd(X.0=0.2),
           cooling.fraction.50=0.5,cooling.type="geometric")
mf <- mif2(mf,start=coef(mf))

## check whether all tests passed
all(get(eval(formals(test))$all))
if (!all(get(eval(formals(test))$all))) stop("Not all tests passed!")

