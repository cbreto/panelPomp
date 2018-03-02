library(panelPomp,quietly=TRUE)

TESTS_PASS <- NULL
test <- function(expr,all="TESTS_PASS",env=parent.frame(),...) 
  panelPomp:::test(expr,all=all,env=env,...)

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

err <- sQuotes("Error : in 'pfilter': 'object' is a required argument\n")
test(try(pfilter(sh=coef(ppo)$sh,sp=coef(ppo)$sp,Np=10),silent=TRUE)[1]==err)
test(try(pfilter(params=coef(ppo),Np=10),silent=TRUE)[1]==err)
rm(err)

test(try(pfilter(ppo,Np=10,tol=rep(1e-7,length(ppo)+1)),silent=TRUE)[1]==sQuotes(
  "Error : in 'pfilter': 'tol' must be a single positive scalar or a",
  " vector of length ",length(ppo),"\n"))

## check whether all tests passed
all(get(eval(formals(test))$all))
if (!all(get(eval(formals(test))$all))) stop("Not all tests passed!")
