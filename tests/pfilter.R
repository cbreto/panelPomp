library(panelPomp)

TESTS_PASS <- NULL
test <- function(...,all="TESTS_PASS",env=parent.frame()) 
  panelPomp:::test(...,all=all,env=env)


ppo <- pompExample(prw,envir=NULL)[[1]]
pos <- as(ppo,"list")
po <- pos[[1]]

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

## check whether all tests passed
all(get(eval(formals(test))$all))
if (!all(get(eval(formals(test))$all))) stop("Not all tests passed!")
