library(panelPomp,quietly=TRUE)

TESTS_PASS <- NULL
test <- function(expr1,expr2,all="TESTS_PASS",env=parent.frame(),...) 
  panelPomp:::test(expr1,expr2,all=all,env=env,...)

ppo <- pompExample(prw,envir=NULL)[[1]]
pos <- as(ppo,"list")
po <- pos[[1]]

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
test(wQuotes(ep,"part of ''shared.start'' is not a shared parameter of ",
             "''object''.",et),
     mif2(panelPomp(unitobjects(ppo)),Np=10,rw.sd=rw.sd(sigmaX=0.05,X.0=0.5),
          cooling.fraction.50=0.5,sh=pparams(ppo)$sh))
test(wQuotes(ep,"part of ''specific.start'' is not a specific parameter of ",
             "''object''.",et),
     mif2(panelPomp(unitobjects(ppo),shared=coef(po)),Np=10,sp=pparams(ppo)$sp,
          rw.sd=rw.sd(sigmaX=0.05,X.0=0.5),cooling.fraction.50=0.5))
## assign parameters
test(conv.rec(as(mif2(ppo,Np=10,rw.sd=rw.sd(X.0=0.2),cooling.fraction.50=0.5,
                      cooling.type="geometric"),"list")[[1]])[1,-(1:2)],
     c(ppo@pParams$sh,get_col(ppo@pParams$sp,1,1)))
test(conv.rec(as(mif2(ppo,sh=2*ppo@pParams$sh,sp=2*ppo@pParams$sp,Np=10,
                      rw.sd=rw.sd(X.0=0.2),cooling.fraction.50=0.5,
                      cooling.type="geometric"),"list")[[1]])[1,-(1:2)],
     2*c(ppo@pParams$sh,get_col(ppo@pParams$sp,1,1)))
test(conv.rec(as(mif2(ppo,sh=2*ppo@pParams$sh,Np=10,rw.sd=rw.sd(X.0=0.2),
                      cooling.fraction.50=0.5,cooling.type="geometric"),
                 "list")[[1]])[1,-(1:2)],
     c(2*ppo@pParams$sh,get_col(ppo@pParams$sp,1,1)))
test(conv.rec(as(mif2(ppo,sp=2*ppo@pParams$sp,Np=10,rw.sd=rw.sd(X.0=0.2),
                      cooling.fraction.50=0.5,cooling.type="geometric"),
                 "list")[[1]])[1,-(1:2)],
     c(ppo@pParams$sh,2*get_col(ppo@pParams$sp,1,1)))
test(conv.rec(as(mif2(ppo,st=lapply(ppo@pParams,`*`,2),Np=10,
                      rw.sd=rw.sd(X.0=0.2),cooling.fraction.50=0.5,
                      cooling.type="geometric"),"list")[[1]])[1,-(1:2)],
     2*c(ppo@pParams$sh,get_col(ppo@pParams$sp,1,1)))
## resolve multiple params
test(conv.rec(as(mif2(ppo,sh=2*ppo@pParams$sh,st=ppo@pParams,Np=10,
                      rw.sd=rw.sd(X.0=0.2),cooling.fraction.50=0.5,
                      cooling.type="geometric"),"list")[[1]])[1,-(1:2)],
     c(2*ppo@pParams$sh,get_col(ppo@pParams$sp,1,1)))
test(conv.rec(as(mif2(ppo,sp=2*ppo@pParams$sp,st=ppo@pParams,Np=10,
                      rw.sd=rw.sd(X.0=0.2),cooling.fraction.50=0.5,
                      cooling.type="geometric"),"list")[[1]])[1,-(1:2)],
     c(ppo@pParams$sh,2*get_col(ppo@pParams$sp,1,1)))
test(wQuotes(ep,"specify either ''start'' only, ''start'' and ''shared.start''",
             ", or ''start'' and ''specific.start''.",et),
     mif2(ppo,sh=2*ppo@pParams$sh,sp=2*ppo@pParams$sp,st=ppo@pParams,
          Np=10,rw.sd=rw.sd(sigmaX=0.05,X.0=0.5),cooling.fraction.50=0.5,
          cooling.type="geometric"))
## wrong unit names
test(wQuotes(ep,"colnames of ''specific'' must be identical to those of ",
             "''object@pParams$specific''.",et),
     quote({sp <- ppo@pParams$sp;colnames(sp) <- paste0(colnames(sp), "_")
     mif2(ppo,Np=10,sp=sp,rw.sd=rw.sd(X.0=0.2),cooling.fraction.50=0.5,
          cooling.type="geometric")}))
## wrong unit-specific names
test(wQuotes(ep,"part of ''specific.start'' is not a specific parameter",
             " of ''object''.",et),
     quote({sp <- ppo@pParams$sp;rownames(sp) <- c("some_wrong_name")
     mif2(ppo,Np=10,sp=sp,rw.sd=rw.sd(X.0=0.2),cooling.fraction.50=0.5,
          cooling.type="geometric")}))
##  wrong shared names
test(wQuotes(ep,"part of ''shared.start'' is not a shared parameter",
             " of ''object''.",et),
     mif2(ppo,Np=10,sh=c(sth=0),rw.sd=rw.sd(X.0=0.2),cooling.fraction.50=0.5,
          cooling.type="geometric"))

## check whether all tests passed
all(get(eval(formals(test))$all))
if (!all(get(eval(formals(test))$all))) stop("Not all tests passed!")
