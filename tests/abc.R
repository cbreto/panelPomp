library(panelPomp,quietly=TRUE)
TESTS_PASS <- NULL 
## define test function
test <- function(expr1,expr2,all="TESTS_PASS",env=parent.frame(),...) 
  panelPomp:::test(expr1,expr2,all=all,env=env,...)

#ppo <- pompExample(prw,envir=NULL)[[1]]
ppo <- panelPompExample(pangomp)
po <- ppo[[1]]

ep <- wQuotes("Error : in ''abc'': ")
et <- wQuotes(" (''abc,panelPomp-method'')\n")

# additional objects
plist <- list(probe.mean(var = "Y", transform = sqrt))
psim <- probe(po, probes = plist, nsim = 1)
scale.dat <- apply(psim$simvals, 2, sd)
hyperparams <- list(min = coef(po)/10, max = coef(po) * 10)
  gompertz.dprior <- function (params, ..., log) {
    f <- sum(dunif(params, min = hyperparams$min, max = hyperparams$max,
                   log = TRUE))
    if (log) f else exp(f)
  }
# add priors to pomps
ppo@unit.objects <- lapply(ppo@unit.objects,
                           pomp,dprior=gompertz.dprior)

## test checks for missing arguments
test(wQuotes(ep,"''object'' is a required argument\n"),
     abc(Nabc = 1,probes = plist, epsilon = 1, scale = scale.dat,
         proposal = mvn.diag.rw(c(r = 0.01, sigma = 0.01, tau = 0.01))))

## test that pomp's c code can be called
test(as.character(.Call("apply_probe_data",po,plist,PACKAGE='panelPomp')),
     '1.03092251026096')

## test that abc runs
test('abcd.ppomp',class(
  abc(ppo,Nabc = 1,probes = plist, epsilon = 1, scale = scale.dat,
      proposal = mvn.diag.rw(c(r = 0.01, sigma = 0.01, tau = 0.01)))
)[1])


## ... and check whether all tests passed
all(get(eval(formals(test))$all))
if (!all(get(eval(formals(test))$all))) stop("Not all tests passed!")
