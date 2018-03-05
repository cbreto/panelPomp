library(panelPomp,quietly=TRUE)

TESTS_PASS <- NULL
test <- function(expr1,expr2,all="TESTS_PASS",env=parent.frame(),...) 
  panelPomp:::test(expr1,expr2,all=all,env=env,...)

ppo <- pompExample(prw,envir=NULL)[[1]]
pos <- as(ppo,"list")
po <- pos[[1]]

## test checks for missing arguments
test(mif2(Np=10,rw.sd=rw.sd(sigmaX=0.05,X.0=0.5),cooling.fraction.50=0.5),
     wQuotes("Error : in ''mif2'': ''object'' is a required argument\n"))
test(mif2(ppo,rw.sd=rw.sd(sigmaX=0.05,X.0=0.5),cooling.fraction.50=0.5),
     wQuotes("Error : in ''mif2'': Missing ''Np'' argument. ",
             "(''mif2,panelPomp-method'')\n")) 
test(mif2(ppo,Np=10,cooling.fraction.50=0.5),
     wQuotes("Error : in ''mif2'': missing ''rw.sd'' argument. ",
             "(''mif2,panelPomp-method'')\n"))
test(mif2(ppo,Np=10,rw.sd=rw.sd(sigmaX=0.05,X.0=0.5)),
     wQuotes("Error : in ''mif2'': Missing ''cooling.fraction.50'' argument. ",
             "(''mif2,panelPomp-method'')\n"))
test(wQuotes("Error : in ''mif2'': names of ''shared.start'' must match those",
             " of ''object@pParams$shared''. (''mif2,panelPomp-method'')\n"),
     mif2(panelPomp(unitobjects(ppo)),Np=10,rw.sd=rw.sd(sigmaX=0.05,X.0=0.5),
          cooling.fraction.50=0.5,sh=pparams(ppo)$sh))

## check whether all tests passed
all(get(eval(formals(test))$all))
if (!all(get(eval(formals(test))$all))) stop("Not all tests passed!")
