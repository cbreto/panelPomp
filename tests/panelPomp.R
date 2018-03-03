library(panelPomp,quietly=TRUE)

TESTS_PASS <- NULL
test <- function(expr1,expr2,all="TESTS_PASS",env=parent.frame(),...) 
  panelPomp:::test(expr1,expr2,all=all,env=env,...)

ppo <- pompExample(prw,envir=NULL)[[1]]
pos <- as(ppo,"list")
pPs <- ppo@pParams

pog <- pompExample(gompertz,envir=NULL)[[1]]

## test checks in validity function for panelPomp class

## test checks for missing arguments in panelPomp function
test(wQuotes("Error : in ''panelPomp'': ''object'' is a required argument.\n"),
     panelPomp())
test(panelPomp(list(a=1)),
     wQuotes("Error : in ''panelPomp'': ''object'' must be a either a ",
             "''panelPomp'' object or a list of ''pomp'' objects.\n"))
test(panelPomp(pos,sh=2*pPs$sh,sp=2*pPs$sp,params=pPs),
     wQuotes("Error : in ''panelPomp'': do not specify all of ''params'', ",
             "''shared'' and ''specific''.\n"))
## test construction of pParams slot ...
## ... when is(object,"pompList") ...
test(panelPomp(pos)@pParams,list(shared=numeric(),specific=sapply(pos,coef)))
test(panelPomp(pos,sh=2*pPs$sh,par=pPs)@pParams$sh,2*pPs$sh)
test(panelPomp(pos,sh=pPs$sh,par=lapply(pPs,`*`,2))@pParams$sp,2*pPs$sp)
test(panelPomp(pos,sp=2*pPs$sp,par=pPs)@pParams$sp,2*pPs$sp)
test(panelPomp(pos,sp=pPs$sp,par=lapply(pPs,`*`,2))@pParams$sh,2*pPs$sh)
test(panelPomp(pos,params=coef(ppo)),ppo)
## ... and when is(object,"pomp")
test(wQuotes("Error : in ''panelPomp'': if ''shared'' is a character vector ",
             "(or NULL), unit specific parameters are taken from ''object''.\n"),
     panelPomp(ppo,sh="sigmaX",params=pPs))
test(panelPomp(ppo,sh="sigmaX")@pParams,
     list(shared=pPs$sh["sigmaX"],specific=rbind(sigmaY=pPs$sh["sigmaY"],pPs$sp)))
test(panelPomp(ppo,sh=NULL)@pParams,
     list(shared=numeric(),
          specific=rbind(sigmaX=pPs$sh["sigmaX"],sigmaY=pPs$sh["sigmaY"],pPs$sp)))
test(panelPomp(ppo,params=coef(ppo)),ppo)

## the panelPomp construction below should be valid! (i.e., all shared)
## it seems to fail to add the unit names as colnames of the otherwise empty sp 
## matrix
#test(identical(
#  names(coef(panelPomp(unitobjects(ppo),params=c(names(pPs$sh),rownames(pPs$sp))))),
#  c(names(pPs$sh),rownames(pPs$sp))))

## check whether all tests passed
all(get(eval(formals(test))$all))
if (!all(get(eval(formals(test))$all))) stop("Not all tests passed!")
