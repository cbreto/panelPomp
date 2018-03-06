library(panelPomp,quietly=TRUE)

TESTS_PASS <- NULL
test <- function(expr1,expr2,all="TESTS_PASS",env=parent.frame(),...) 
  panelPomp:::test(expr1,expr2,all=all,env=env,...)

ppo <- pompExample(prw,envir=NULL)[[1]]
pos <- as(ppo,"list")
pPs <- pparams(ppo)

pog <- pompExample(gompertz,envir=NULL)[[1]]
all_sh <- c(pPs$sh,get_col(pPs$sp,col=1,rows=seq_along(dim(pPs$sp)[1])))
noparams <- lapply(unitobjects(ppo),pomp,params=numeric(0))
somepars <- setNames(c(as(ppo,"list"),noparams[[1]]),c(names(ppo),"rw3"))

## test checks in validity function for panelPomp class
#test(new("panelPomp",unit.objects=list(pog)),
#     wQuotes("Error in validObject(.Object) : \n  invalid class ",
#             "*panelPomp* object: ''unit.object''",
#             "must have names\n"))
#
#new("panelPomp",unit.objects=list(a=1))

# test ''pomps'' with same parameters
# one unit panels
test(pparams(panelPomp(noparams[1])),pparams(new("panelPomp")))
test(pparams(panelPomp(somepars[1]))$shared,pparams(new("panelPomp"))$shared)
test(pparams(panelPomp(somepars[1]))$sp[names(all_sh),1],all_sh)

#unit.objects <- noparams

#unit.objects <- unitobjects(ppo)
#rw <- as(ppo,"list")[[1]]
#unit.objects[[1]] <- pomp(rw,params=c(X.0=2,sigmaX=1.2,sigmaY=3.1))
#unit.objects[[2]] <- pomp(rw,params=c(sigmaY=0.31,sigmaX=0.12,X.0=0.2))
#unit.objects <- somepars

ep <- "Error : in ''panelPomp'': "
## test checks for missing arguments in panelPomp function
test(wQuotes(ep,"''object'' is a required argument.\n"),
     panelPomp())
test(try(panelPomp(list(a=1)),silent=TRUE)[1],
     wQuotes(ep,"''object'' must be a either a ''panelPomp'' object or a",
             " list of ''pomp'' objects.","\n"))
test(panelPomp(pos,sh=2*pPs$sh,sp=2*pPs$sp,params=pPs),
     wQuotes(ep,"do not specify all of ''params'', ''shared'' and ",
             "''specific''.\n"))
## test construction of pParams slot ...
## ... when is(object,"pompList") ...
test(panelPomp(pos)@pParams,list(shared=numeric(),specific=sapply(pos,coef)))
test(panelPomp(pos,sh=2*pPs$sh,par=pPs)@pParams$sh,2*pPs$sh)
test(panelPomp(pos,sh=pPs$sh,par=lapply(pPs,`*`,2))@pParams$sp,2*pPs$sp)
test(panelPomp(pos,sp=2*pPs$sp,par=pPs)@pParams$sp,2*pPs$sp)
test(panelPomp(pos,sp=pPs$sp,par=lapply(pPs,`*`,2))@pParams$sh,2*pPs$sh)
test(panelPomp(pos,params=coef(ppo)),ppo)
# noparams
test(pparams(panelPomp(noparams)),pparams(new("panelPomp")))
test(unitobjects(panelPomp(noparams)),lapply(pos,`coef<-`,value=numeric(0)))
# someparams
test(panelPomp(somepars),
     wQuotes(ep,"the parameter names of all ''pomp'' objects ",
             "must be the same (albeit ''pomp'' codes can ignore ",
             "parameters that are irrelevant to any given unit)\n"))
## ... and when is(object,"panelPomp")
test(wQuotes(ep,"if ''shared'' is a character vector (or NULL), unit specific ",
             "parameters are taken from ''object''.\n"),
     panelPomp(ppo,sh="sigmaX",params=pPs))
test(panelPomp(ppo,sh="sigmaX")@pParams,
     list(shared=pPs$sh["sigmaX"],specific=rbind(sigmaY=pPs$sh["sigmaY"],pPs$sp)))
test(panelPomp(ppo,sh=NULL)@pParams,
     list(shared=numeric(),
          specific=rbind(sigmaX=pPs$sh["sigmaX"],sigmaY=pPs$sh["sigmaY"],pPs$sp)))
test(panelPomp(ppo,params=coef(ppo)),ppo)
# all_sh
test(pparams(panelPomp(object=unitobjects(ppo),params=all_sh)),
     list(shared=all_sh,specific=
            array(numeric(0),dim=c(0,length(pos)),dimnames=list(param=character(0),
                                                                unit=names(pos)))
     ))

## check whether all tests passed
all(get(eval(formals(test))$all))
if (!all(get(eval(formals(test))$all))) stop("Not all tests passed!")
