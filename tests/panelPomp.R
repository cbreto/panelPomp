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
  try(panelPomp(pos,sh=2*pPs$sh,sp=2*pPs$sp,params=pPs),silent=TRUE)[1]==
    sQuotes("Error : in 'panelPomp': specify either 'params' only, 'params' ",
            "and 'shared', or 'params' and 'specific'.\n") -> PASSES#?
) -> TESTS_PASS
PASSES#?
# does panelPomp replace shared and specific parameters as expected?
c(TESTS_PASS,identical(panelPomp(pos,sh=2*pPs$sh,par=pPs)@pParams$sh,2*pPs$sh
) -> PASSES) -> TESTS_PASS
PASSES#?
c(TESTS_PASS,
  identical(panelPomp(pos,sh=pPs$sh,par=lapply(pPs,`*`,2))@pParams$sp,2*pPs$sp
  ) -> PASSES) -> TESTS_PASS
PASSES#?
c(TESTS_PASS,identical(panelPomp(pos,sp=2*pPs$sp,par=pPs)@pParams$sp,2*pPs$sp
) -> PASSES) -> TESTS_PASS
PASSES#?
c(TESTS_PASS,
  identical(panelPomp(pos,sp=pPs$sp,par=lapply(pPs,`*`,2))@pParams$sh,2*pPs$sh
  ) -> PASSES) -> TESTS_PASS
PASSES#?

stopifnot(all(TESTS_PASS))
all(TESTS_PASS)#?
