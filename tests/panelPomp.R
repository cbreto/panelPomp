library(panelPomp)

TESTS_PASS <- NULL

ppo <- pompExample(prw,envir=NULL)[[1]]
pos <- as(ppo,"list")
po <- pos[[1]]
pPs <- ppo@pParams

pog <- pompExample(gompertz,envir=NULL)[[1]]

# test validity function

# test panelPomp,list-method (move to testing panelPomp function)
#c(TESTS_PASS,
#  try(panelPomp(),silent=TRUE)[1]==sQuotes(
#    "Error : in 'panelPomp': 'object' is a required argument.\n") -> PASSES#?
#) -> TESTS_PASS
#PASSES#?
c(TESTS_PASS,
  try(panelPomp(list(a=1)),silent=TRUE)[1]==sQuotes(
    "Error : in 'panelPomp': 'object' must be a list of 'pomp' ",
    "objects.\n") -> PASSES#?
) -> TESTS_PASS
PASSES#?
c(TESTS_PASS,
  try(panelPomp(pos,sh=pPs$shared,sp=pPs$sp,params=pPs),silent=TRUE)[1]==sQuotes(
    "Error : in 'panelPomp': specify either 'params' only, 'params' and ",
    "'shared', or 'params' and 'specific'.\n") -> PASSES#?
) -> TESTS_PASS
PASSES#?

stopifnot(all(TESTS_PASS))
all(TESTS_PASS)#?
