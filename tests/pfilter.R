library(panelPomp,quietly=TRUE)

TESTS_PASS <- NULL
test <- function(expr1,expr2,all="TESTS_PASS",env=parent.frame(),...)
  panelPomp:::test(expr1,expr2,all=all,env=env,...)

ppo <- pompExample(prw,envir=NULL)[[1]]
pos <- as(ppo,"list")
po <- pos[[1]]
pPs <- pparams(ppo)

ep <- wQuotes("Error : in ''pfilter'': ")

test(wQuotes(ep,"''object'' is a required argument\n"),
  pfilter(sh=coef(ppo)$sh,sp=coef(ppo)$sp,Np=10))
test(wQuotes(ep,"''object'' is a required argument\n"),
  pfilter(params=coef(ppo),Np=10))
test(wQuotes(ep,"''tol'' must be a single positive ",
             "scalar or a vector of length ",length(ppo),"\n"),
     pfilter(ppo,Np=10,tol=rep(1e-7,length(ppo)+1)))
test(wQuotes(ep,"names of ''shared'' must match those of ",
             "''object@pParams$shared''.\n"),
     pfilter(panelPomp(unitobjects(ppo)),sh=pparams(ppo)$sh,Np=10))
test(wQuotes(ep,"Missing ''Np'' argument.\n"),pfilter(ppo))
## assign parameters
test(coef(pfilter(ppo,Np=10)),coef(ppo))
test(coef(as(pfilter(ppo,sh=2*ppo@pParams$sh,sp=2*ppo@pParams$sp,Np=10),
             "list")[[1]]),c(2*ppo@pParams$sh,2*get_col(ppo@pParams$sp,1,1)))
test(coef(as(pfilter(ppo,sh=2*ppo@pParams$sh,Np=10),"list")[[1]]),
     c(2*ppo@pParams$sh,get_col(ppo@pParams$sp,1,1)))
test(coef(as(pfilter(ppo,sp=2*ppo@pParams$sp,Np=10),"list")[[1]]),
     c(ppo@pParams$sh,2*get_col(ppo@pParams$sp,1,1)))
test(coef(as(pfilter(ppo,params=lapply(ppo@pParams,`*`,2),Np=10),
             "list")[[1]]),2*c(ppo@pParams$sh,get_col(ppo@pParams$sp,1,1)))
## resolve multiple params
test(coef(as(pfilter(ppo,sh=2*ppo@pParams$sh,params=ppo@pParams,Np=10),
             "list")[[1]]),c(2*ppo@pParams$sh,get_col(ppo@pParams$sp,1,1)))
test(coef(as(pfilter(ppo,sp=2*ppo@pParams$sp,params=ppo@pParams,Np=10),
             "list")[[1]]),c(ppo@pParams$sh,2*get_col(ppo@pParams$sp,1,1)))
test(wQuotes(ep,"specify either ''params'' only, ''params'' and ''shared'' , ",
             "or ''params'' and ''specific''.\n"),
     pfilter(ppo,sh=2*ppo@pParams$sh,sp=2*ppo@pParams$sp,params=ppo@pParams,
             Np=10))
## wrong unit names
test(wQuotes(ep,"colnames of ''specific'' must be identical to those of ",
             "''object@pParams$specific''.\n"),
     quote({sp <- ppo@pParams$sp;colnames(sp) <- paste0(colnames(sp), "_")
     pfilter(ppo,sp=sp,Np=10)}))
## wrong unit-specific names
test(wQuotes(ep,"rownames of ''specific'' must match those of ",
             "''object@pParams$specific''.\n"),
     quote({sp <- ppo@pParams$sp;rownames(sp) <- c("some_wrong_name")
     pfilter(ppo,sp=sp,Np=10)}))
##  wrong shared names
test(wQuotes(ep,"names of ''shared'' must match those of ",
             "''object@pParams$shared''.\n"),
     pfilter(ppo,sh=c(sth = 0),Np=10))

## check whether all tests passed
all(get(eval(formals(test))$all))
if (!all(get(eval(formals(test))$all))) stop("Not all tests passed!")
