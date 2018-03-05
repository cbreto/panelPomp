library(panelPomp,quietly=TRUE)

TESTS_PASS <- NULL
test <- function(expr1,expr2,all="TESTS_PASS",env=parent.frame(),...) 
  panelPomp:::test(expr1,expr2,all=all,env=env,...)

ppo <- pompExample(prw,envir=NULL)[[1]]
pos <- as(ppo,"list")
pPs <- pparams(ppo)

pog <- pompExample(gompertz,envir=NULL)[[1]]
all_sh <- c(pPs$sh,get_col(pPs$sp,col=1,rows=seq_along(dim(pPs$sp)[1])))
pos_noparams <- lapply(unitobjects(ppo),function (pp) pomp(pp,params=numeric(0)))

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
## ... and when is(object,"panelPomp")
test(wQuotes("Error : in ''panelPomp'': if ''shared'' is a character vector ",
             "(or NULL), unit specific parameters are taken from ''object''.\n"),
     panelPomp(ppo,sh="sigmaX",params=pPs))
test(panelPomp(ppo,sh="sigmaX")@pParams,
     list(shared=pPs$sh["sigmaX"],specific=rbind(sigmaY=pPs$sh["sigmaY"],pPs$sp)))
test(panelPomp(ppo,sh=NULL)@pParams,
     list(shared=numeric(),
          specific=rbind(sigmaX=pPs$sh["sigmaX"],sigmaY=pPs$sh["sigmaY"],pPs$sp)))
test(panelPomp(ppo,params=coef(ppo)),ppo)
# all parameters shared
test(pparams(panelPomp(object=unitobjects(ppo),params=all_sh)),
     list(shared=all_sh,specific=
            array(numeric(0),dim=c(0,length(pos)),dimnames=list(param=character(0),
                                                                unit=names(pos)))
     ))
# empty params in pompList
test(panelPomp(pos_noparams),
     wQuotes("Error : in ''panelPomp'': when parameters come from a list of ",
             "''pomps,'' all ''pomps'' must have non-empty ''params'' slot\n"))
## check whether all tests passed
all(get(eval(formals(test))$all))
if (!all(get(eval(formals(test))$all))) stop("Not all tests passed!")
