library(panelPomp)

TESTS_PASS <- NULL

ppo <- pompExample(prw,envir=NULL)[[1]]
pos <- as(ppo,"list")
po <- pos[[1]]

# check reproducibility over a range of randomly chosen seeds
seeds <- c(21125715L,81902071L,86680005L,29971689L,73376915L)
for (s in seq_along(seeds)) {
set.seed(seeds[s])
mf <- mif2(ppo,Np=10,rw.sd=rw.sd(sigmaX=0.05,X.0=0.5),cooling.fraction.50=0.5)
print(mf@pParams)
print(unitlogLik(mf))
print(logLik(mf))
}
TESTS_PASS <- c(
  TESTS_PASS,# for missing object
  try(mif2(Np=10,rw.sd=rw.sd(sigmaX=0.05,X.0=0.5),cooling.fraction.50=0.5),
      silent=TRUE)[1]==sQuotes(
        "Error : in 'mif2': 'object' is a required argument\n") -> PASSES#?
)
PASSES#?
TESTS_PASS <- c(
  TESTS_PASS,# for missing Np
  try(mif2(ppo,rw.sd=rw.sd(sigmaX=0.05,X.0=0.5),cooling.fraction.50=0.5),
      silent=TRUE)[1]
  ==
  sQuotes("Error : in 'panelPomp::mif2': Missing 'Np' argument.\n") -> PASSES#?
)
PASSES#?
TESTS_PASS <- c(
  TESTS_PASS,# for missing rw.sd
  try(mif2(ppo,Np=10,cooling.fraction.50=0.5),silent=TRUE)[1]
  ==
  sQuotes("Error : in 'panelPomp::mif2': missing 'rw.sd' argument.\n") -> PASSES#?
)
PASSES#?
TESTS_PASS <- c(
  TESTS_PASS,# for missing object
  try(mif2(ppo,Np=10,rw.sd=rw.sd(sigmaX=0.05,X.0=0.5)),silent=TRUE)[1]
  ==
  sQuotes("Error : in 'panelPomp::mif2': Missing 'cooling.fraction.50'",
          " argument.\n") -> PASSES#?
)
PASSES#?

all(TESTS_PASS)#?
