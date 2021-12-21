library(panelPomp,quietly=TRUE)

TESTS_PASS <- NULL
test <- function(expr1,expr2,all="TESTS_PASS",env=parent.frame(),...)
  panelPomp:::test(expr1,expr2,all=all,env=env,...)

ppo <- panelRandomWalk(U=2,N=5)

##  check whether pfiltering a pfilterd.ppomp returns the same object
set.seed(21125715L)
ppf <- pfilter(ppo,Np=10)
logLik(ppf)
unitlogLik(ppf)
set.seed(21125715L)
ppf_ <- pfilter(ppf,Np=10)
logLik(ppf_)
unitlogLik(ppf_)

test(ppf,ppf_)


## check whether all tests passed
all(get(eval(formals(test))$all))
if (!all(get(eval(formals(test))$all))) stop("Not all tests passed!")
