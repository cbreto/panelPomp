library(panelPomp,quietly=TRUE)

TESTS_PASS <- NULL
test <- function(expr,all="TESTS_PASS",env=parent.frame(),...) 
  panelPomp:::test(expr,all=all,env=env,...)

ppo <- pompExample(prw,envir=NULL)[[1]]
pos <- as(ppo,"list")
po <- pos[[1]]

## check reproducibility over a range of randomly chosen seeds
seeds <- c(21125715L,81902071L,86680005L,29971689L,73376915L)
for (s in seq_along(seeds)) {
set.seed(seeds[s])
mf <- mif2(ppo,Np=10,rw.sd=rw.sd(sigmaX=0.05,X.0=0.5),cooling.fraction.50=0.5)
print(mf@pParams)
print(unitlogLik(mf))
print(logLik(mf))
}
## test checks for missing arguments
test(try(mif2(Np=10,rw.sd=rw.sd(sigmaX=0.05,X.0=0.5),cooling.fraction.50=0.5),
         silent=TRUE)[1]==sQuotes(
           "Error : in 'mif2': 'object' is a required argument\n"))
test(try(
  mif2(ppo,rw.sd=rw.sd(sigmaX=0.05,X.0=0.5),cooling.fraction.50=0.5),
  silent=TRUE)[1]==sQuotes(
    "Error : in 'mif2': Missing 'Np' argument. ('mif2,panelPomp-method')\n")) 
test(try(mif2(ppo,Np=10,cooling.fraction.50=0.5),silent=TRUE)[1]==sQuotes(
  "Error : in 'mif2': missing 'rw.sd' argument. ('mif2,panelPomp-method')\n"))
test(try(mif2(ppo,Np=10,rw.sd=rw.sd(sigmaX=0.05,X.0=0.5)),silent=TRUE
         )[1]==sQuotes("Error : in 'mif2': Missing 'cooling.fraction.50' ",
                       "argument. ('mif2,panelPomp-method')\n"))
## check whether all tests passed
all(get(eval(formals(test))$all))
if (!all(get(eval(formals(test))$all))) stop("Not all tests passed!")
