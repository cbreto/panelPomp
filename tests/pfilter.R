if (file.exists("_options.R")) source("_options.R")
library(panelPomp,quietly=TRUE)

TESTS_PASS <- NULL
test <- function(expr1,expr2,all="TESTS_PASS",env=parent.frame(),...)
  panelPomp:::test(expr1,expr2,all=all,env=env,...)

ppo <- panelRandomWalk(U=1,N=7)
pos <- as(ppo,"list")
po <- pos[[1]]
pPs <- pparams(ppo)

ep <- wQuotes("Error : in ''pfilter'': ")

test(wQuotes(ep,"''data'' is a required argument.\n"),
  pfilter(sh=coef(ppo)$sh,sp=coef(ppo)$sp,Np=10))
test(wQuotes(ep,"''data'' is a required argument.\n"),
  pfilter(params=coef(ppo),Np=10))
test(wQuotes(ep,"names of ''shared'' must match those of ",
  "''object@shared''.\n"),
  pfilter(panelPomp(unitobjects(ppo)),sh=pparams(ppo)$sh,Np=10))
test(wQuotes(ep,"Missing ''Np'' argument.\n"),pfilter(ppo))

# Testing error message if params argument is list without shared / specific elements
test(
  wQuotes(ep, "''params'' must be a list containing ''shared'' and ",
          "''specific'' elements, or a named numeric vector.\n"),
  pfilter(
    ppo, Np = 10, params = list(shard = ppo@shared, spesific = ppo@specific)
  )
)
test(
  wQuotes(ep, "''params'' must be a list containing ''shared'' and ",
  "''specific'' elements, or a named numeric vector.\n"),
  pfilter(
    ppo, Np = 10, params = list(shared = ppo@shared)
  )
)
test(
  wQuotes(ep, "''params'' must be a list containing ''shared'' and ",
          "''specific'' elements, or a named numeric vector.\n"),
  pfilter(
    ppo, Np = 10, params = list(specific = ppo@specific)
  )
)


## assign parameters
test(coef(pfilter(ppo,Np=10)),coef(ppo))
test(coef(as(pfilter(ppo,sh=2*ppo@shared,sp=2*ppo@specific,Np=10),
  "list")[[1]]),c(2*ppo@shared,2*get_col(ppo@specific,1,1)))
test(coef(as(pfilter(ppo,sh=2*ppo@shared,Np=10),"list")[[1]]),
  c(2*ppo@shared,get_col(ppo@specific,1,1)))
test(coef(as(pfilter(ppo,sp=2*ppo@specific,Np=10),"list")[[1]]),
  c(ppo@shared,2*get_col(ppo@specific,1,1)))
test(coef(as(pfilter(ppo,Np=10,
  params=list(shared=2*ppo@shared,specific=2*ppo@specific)),
  "list")[[1]]),2*c(ppo@shared,get_col(ppo@specific,1,1)))
## resolve multiple params
test(coef(as(pfilter(ppo,sh=2*ppo@shared,Np=10,
  params=list(shared=ppo@shared,specific=ppo@specific)),
  "list")[[1]]),c(2*ppo@shared,get_col(ppo@specific,1,1)))
test(coef(as(pfilter(ppo,sp=2*ppo@specific,
  params=list(shared=ppo@shared,specific=ppo@specific),Np=10),
  "list")[[1]]),c(ppo@shared,2*get_col(ppo@specific,1,1)))
test(wQuotes(ep,"specify either ''params'' only, ''params'' and ''shared'' ,",
  " or ''params'' and ''specific''.\n"),
  pfilter(ppo,sh=2*ppo@shared,sp=2*ppo@specific,
    params=list(shared=ppo@shared,specific=ppo@specific),
    Np=10))
## provide params without shared nor specific
set.seed(21125715L)
ppf <- pfilter(ppo,sh=ppo@shared,sp=ppo@specific,Np=10)
set.seed(21125715L)
ppf_<-pfilter(ppo,params=list(shared=ppo@shared,specific=ppo@specific),Np=10)
test(logLik(ppf),logLik(ppf_))
numeric_names <- setNames(rep(1,3),c(names(ppo@shared),"X.0[rw1]"))
test(pPs,pParams(numeric_names))
set.seed(21125715L)
ppf__<-pfilter(ppo,params=numeric_names,Np=10)
test(logLik(ppf),logLik(ppf__))
## wrong unit names
test(wQuotes(ep,"colnames of ''specific'' must match those of ",
  "''object@specific''.\n"),
  quote({sp <- ppo@specific;colnames(sp) <- paste0(colnames(sp), "_")
  pfilter(ppo,sp=sp,Np=10)}))
## wrong unit-specific names
test(wQuotes(ep,"rownames of ''specific'' must match those of ",
  "''object@specific''.\n"),
  quote({sp <- ppo@specific;rownames(sp) <- c("some_wrong_name")
  pfilter(ppo,sp=sp,Np=10)}))
##  wrong shared names
test(wQuotes(ep,"names of ''shared'' must match those of ",
  "''object@shared''.\n"),
  pfilter(ppo,sh=c(sth = 0),Np=10))

ppf <- pfilter(ppo,Np=10)
test(dim(as(ppf,"data.frame")),c(7L,5L))
test(names(as(ppf,"data.frame")),c("t", "Y", "ess", "cond.logLik", "unit"))

## test whether matching by unit name works
  g <- panelGompertz(U=10,N=3)
  ## check that previously broken code runs without error
  g0 <- pfilter(g, Np=10,
    shared=pParams(coef(g))$shared,
    specific=pParams(coef(g))$specific)
  ## a longer stronger test
  long_test <- FALSE
  if(long_test){
    set.seed(12323218)
    g1 <- pfilter(g, Np=10000,
      shared=pParams(coef(g))$shared,
      specific=pParams(coef(g))$specific)
    g2 <- pfilter(g, Np=10000)
    test(abs(logLik(p1)-logLik(p2))<0.2, TRUE)
  }

## check whether all tests passed
all(get(eval(formals(test))$all))
if (!all(get(eval(formals(test))$all))) stop("Not all tests passed!")
