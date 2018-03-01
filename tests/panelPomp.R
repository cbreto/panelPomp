library(panelPomp,quietly=TRUE)

TESTS_PASS <- NULL
test <- function(expr,all="TESTS_PASS",env=parent.frame(),...) 
  panelPomp:::test(expr,all=all,env=env,...)

ppo <- pompExample(prw,envir=NULL)[[1]]
pos <- as(ppo,"list")
pPs <- ppo@pParams

pog <- pompExample(gompertz,envir=NULL)[[1]]

## test checks in validity function for panelPomp class

## test checks for missing arguments in panelPomp function
test(try(panelPomp(),silent=TRUE)[1]==sQuotes(
  "Error : in 'panelPomp': 'object' is a required argument.\n"))
test(try(panelPomp(list(a=1)),silent=TRUE)[1]==sQuotes(
  "Error : in 'panelPomp': 'object' must be a either a 'panelPomp' ",
  "object or a list of 'pomp' objects.\n"))
test(try(panelPomp(pos,sh=2*pPs$sh,sp=2*pPs$sp,params=pPs),silent=TRUE)[1]==
       sQuotes("Error : in 'panelPomp': do not specify all of 'params', ",
               "'shared' and 'specific'.\n"))
## test construction of pParams slot ...
## ... when is(object,"pompList") ...
test(identical(panelPomp(pos)@pParams,
               list(shared=numeric(),specific=sapply(pos,coef))))
test(identical(panelPomp(pos,sh=2*pPs$sh,par=pPs)@pParams$sh,2*pPs$sh)) 
test(identical(panelPomp(pos,sh=pPs$sh,par=lapply(pPs,`*`,2))@pParams$sp,
               2*pPs$sp))
test(identical(panelPomp(pos,sp=2*pPs$sp,par=pPs)@pParams$sp,2*pPs$sp)) 
test(identical(panelPomp(pos,sp=pPs$sp,par=lapply(pPs,`*`,2))@pParams$sh,
               2*pPs$sh))

## ... and when is(object,"pomp")
test(try(panelPomp(ppo,sh="sigmaX",params=pPs),silent=TRUE)[1]==
       sQuotes("Error : in 'panelPomp': if 'shared' is a character vector (or",
               " NULL), unit specific parameters are taken from 'object'.\n"))
test(identical(
  panelPomp(ppo,sh="sigmaX")@pParams,
  list(shared=pPs$sh["sigmaX"],specific=rbind(sigmaY=pPs$sh["sigmaY"],pPs$sp))))
test(identical(
  panelPomp(ppo,sh=NULL)@pParams,
  list(shared=numeric(),
       specific=rbind(sigmaX=pPs$sh["sigmaX"],sigmaY=pPs$sh["sigmaY"],pPs$sp))))

## check whether all tests passed
all(get(eval(formals(test))$all))
if (!all(get(eval(formals(test))$all))) stop("Not all tests passed!")
