if (file.exists("_options.R")) source("_options.R")
library(panelPomp,quietly=TRUE)

TESTS_PASS <- NULL
test <- function(expr1,expr2,all="TESTS_PASS",env=parent.frame(),...)
  panelPomp:::test(expr1,expr2,all=all,env=env,...)

pg <- panelGompertz(U=5,N=3)
pg <- panelPomp(pg[1:3])
pgl <- as(pg,"list")
g <- pgl[[1]]; coef(g) <- c(pparams(pg)$sh, pparams(pg)$sp[,1])
coef(g)
coef(pg)
coef(panelPomp(pg,shared=NULL))
coef(panelPomp(pg,specific=names(coef(g))))
coef(panelPomp(pg,shared=c(r=0.1,sigma=0.1,K=1)))
coef(panelPomp(pg,specific=c("r","K","tau","X.0")))
try(panelPomp(pg,specific=c("tau","X.0"),params=c(r=3,K=1)))
stopifnot(all.equal(coef(panelPomp(pg,params=coef(g))),coef(g)))
try(panelPomp(pg,params=list(bob=3,nancy="A")))
try(panelPomp(pg,shared=c("r","K")))
try(panelPomp(pg,specific=c(0.3)))
coef(panelPomp(pg,specific=c(r=0.3)))
try(panelPomp(pg,specific="h"))
try(panelPomp(pg,specific="h",shared=c(r=33)))
try(panelPomp(pg,specific=list(r=0.3,K=9)))
coef(panelPomp(pg,specific=c("tau","X.0"),shared=c(r=3,K=1)))
try(panelPomp(setNames(pgl,c("a","b",""))))
coef(pgl[[2]]) <- c(h=3)
try(po <- panelPomp(pgl))
try(panelPomp(pgl[[1]]))



ppo <- panelRandomWalk(U=2,N=5)
pos <- as(ppo,"list")
pPs <- pparams(ppo)
all_sh <- c(pPs$sh,get_col(pPs$sp,col=1,rows=seq_along(dim(pPs$sp)[1])))

noparams <- lapply(unitobjects(ppo),pomp,params=numeric(0))


ep <- "Error : in ''panelPomp'': "
## test checks for missing arguments in panelPomp function
test(wQuotes(ep,"''object'' is a required argument.\n"),
     panelPomp())
test(wQuotes(ep,"''object'' must be either a ''panelPomp'' object or a list of",
             " ''pomp'' objects.","\n"),
     panelPomp(list(a=1)))
test(wQuotes(ep,"specify EITHER ''params'' OR ''shared'' and/or ''specific''.",
             "\n"),
     panelPomp(pos,sh=2*pPs$sh,sp=2*pPs$sp,params=pPs))


## test construction of pParams slot ...
## ... when is(object,"pompList") ...
test(as(panelPomp(pos,params=coef(ppo)),"data.frame"),
     as(ppo,"data.frame"))
# noparams
test(obs(unitobjects(panelPomp(noparams))[[1]]),
     obs(lapply(pos,`coef<-`,value=numeric(0))[[1]]) )
# someparams
test(as(panelPomp(ppo,params=coef(ppo)),"data.frame"),
     as(ppo,"data.frame"))

test(wQuotes(ep,"''object'' is a required argument.\n"),
     panelPomp(shared=ppo@shared,specific=ppo@specific))

test(wQuotes(ep,
             "column names of ''specific'' must correspond to names of units",
             "\n"),
     {sp_rw2 <- ppo@specific
     colnames(sp_rw2) <- c("rw1","rw3")
     panelPomp(ppo,shared=ppo@shared,specific=sp_rw2)})

test(wQuotes(ep,"a parameter cannot be both shared and specific!\n"),
     panelPomp(ppo,shared=ppo@shared,specific=c("X.0","sigmaX")))

test(wQuotes(
  "Error in validObject(.Object) : \n  invalid class *panelPomp* object: a ",
  "parameter cannot be both shared and specific!\n"),
  {sp_sigma <- ppo@specific
  rownames(sp_sigma) <- "sigmaX"
  panelPomp(ppo,shared=ppo@shared,specific=sp_sigma)})



## check whether all tests passed
all(get(eval(formals(test))$all))
if (!all(get(eval(formals(test))$all))) stop("Not all tests passed!")
