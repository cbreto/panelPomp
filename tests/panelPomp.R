library(panelPomp,quietly=TRUE)

TESTS_PASS <- NULL
test <- function(expr1,expr2,all="TESTS_PASS",env=parent.frame(),...) 
  panelPomp:::test(expr1,expr2,all=all,env=env,...)

pg <- panelPompExample(pangomp)
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



ppo <- panelPomp:::pompExample(prw,envir=NULL)[[1]]
pos <- as(ppo,"list")
pPs <- pparams(ppo)
all_sh <- c(pPs$sh,get_col(pPs$sp,col=1,rows=seq_along(dim(pPs$sp)[1])))

noparams <- lapply(unitobjects(ppo),pomp,params=numeric(0))
#somepars <- setNames(c(as(ppo,"list"),noparams[[1]]),c(names(ppo),"rw3"))

# test ''pomps'' with same parameters
# one unit panels
#test(pparams(panelPomp(noparams[1])),pparams(new("panelPomp")))
#test(pparams(panelPomp(somepars[1]))$shared,pparams(new("panelPomp"))$shared)
#test(pparams(panelPomp(ppo[1]))$sp[names(all_sh),1],all_sh)

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
#test(panelPomp(pos)@pParams,list(shared=numeric(),specific=sapply(pos,coef)))
#test(panelPomp(pos,sh=pPs$sh,par=lapply(pPs,`*`,2))@pParams$sp,2*pPs$sp)
#test(panelPomp(pos,sp=2*pPs$sp,par=pPs)@pParams$sp,2*pPs$sp)
#test(panelPomp(pos,sp=pPs$sp,par=lapply(pPs,`*`,2))@pParams$sh,2*pPs$sh)
test(panelPomp(pos,params=coef(ppo)),ppo)
# noparams
#test(pparams(panelPomp(noparams)),pparams(new("panelPomp")))
test(unitobjects(panelPomp(noparams)),lapply(pos,`coef<-`,value=numeric(0)))
# someparams
#test(wQuotes(
#  ep,"the parameter names of all ''pomp'' objects must be the same (albeit ",
#  "''pomp'' codes can ignore parameters that are irrelevant to any given ",
#  "unit)\n"),
#  panelPomp(somepars))
## ... and when is(object,"panelPomp")
#test(wQuotes(ep,"if ''shared'' is a character vector (or NULL), unit specific ",
#             "parameters are taken from ''object''.\n"),
#     panelPomp(ppo,sh="sigmaX",params=pPs))
#test(panelPomp(ppo,sh="sigmaX")@pParams,
#     list(shared=pPs$sh["sigmaX"],specific=rbind(sigmaY=pPs$sh["sigmaY"],pPs$sp)))
#test(panelPomp(ppo,sh=NULL)@pParams,
#     list(shared=numeric(),
#          specific=rbind(sigmaX=pPs$sh["sigmaX"],sigmaY=pPs$sh["sigmaY"],pPs$sp)))
test(panelPomp(ppo,params=coef(ppo)),ppo)

# all_sh
#test(pparams(panelPomp(object=unitobjects(ppo),params=all_sh)),
#     list(shared=all_sh,specific=
#            array(numeric(0),dim=c(0,length(pos)),
#                  dimnames=list(param=character(0),unit=names(pos)))))


test(wQuotes(ep,"''object'' is a required argument.\n"),
     panelPomp(shared=ppo@shared,specific=ppo@specific))

test(wQuotes(ep,
             "column names of ''specific'' must correspond to names of units",
             "\n"),
     {sp_rw3 <- ppo@specific
     dimnames(sp_rw3)$unit <- c("rw1","rw3")
     panelPomp(ppo,shared=ppo@shared,specific=sp_rw3)})

test(wQuotes(ep,"a parameter cannot be both shared and specific!\n"),
     panelPomp(ppo,shared=ppo@shared,specific=c("X.0","sigmaX")))

test(wQuotes(
  "Error in validObject(.Object) : \n  invalid class *panelPomp* object: a ",
  "parameter cannot be both shared and specific!\n"),
  {sp_sigma <- ppo@specific
  dimnames(sp_sigma)$param <- "sigmaX"
  panelPomp(ppo,shared=ppo@shared,specific=sp_sigma)})



## check whether all tests passed
all(get(eval(formals(test))$all))
if (!all(get(eval(formals(test))$all))) stop("Not all tests passed!")
