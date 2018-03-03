library(panelPomp,quietly=TRUE)

TESTS_PASS <- NULL
test <- function(expr1,expr2,all="TESTS_PASS",env=parent.frame(),...) 
  panelPomp:::test(expr1,expr2,all=all,env=env,...)

ppo <- pompExample(prw,envir=NULL)[[1]]
pos <- as(ppo,"list")
po <- pos[[1]]
pPs <- pparams(ppo)

# check reproducibility over a range of randomly chosen seeds
seeds <- c(21125715L,81902071L,86680005L,29971689L,73376915L)
for (s in seq_along(seeds)) {
set.seed(seeds[s])
pf <- pfilter(ppo,Np=10)
print(unitlogLik(pf))
print(logLik(pf))
}

test(wQuotes("Error : in ''pfilter'': ''object'' is a required argument\n"),
  pfilter(sh=coef(ppo)$sh,sp=coef(ppo)$sp,Np=10))
test(wQuotes("Error : in ''pfilter'': ''object'' is a required argument\n"),
  pfilter(params=coef(ppo),Np=10))

test(wQuotes("Error : in ''pfilter'': ''tol'' must be a single positive ",
             "scalar or a vector of length ",length(ppo),"\n"),
     pfilter(ppo,Np=10,tol=rep(1e-7,length(ppo)+1)))
test(wQuotes("Error : in ''pfilter'': names of ''shared'' must match those of ",
             "''object@pParams$shared''.\n"),
     pfilter(panelPomp(unitobjects(ppo)),sh=pparams(ppo)$sh,Np=10))

## check whether all tests passed
all(get(eval(formals(test))$all))
if (!all(get(eval(formals(test))$all))) stop("Not all tests passed!")
