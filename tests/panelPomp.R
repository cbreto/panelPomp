library(panelPomp)

TESTS_PASS <- NULL

ppo <- pompExample(prw,envir=NULL)[[1]]
pos <- as(ppo,"list")
po <- pos[[1]]
pPs <- ppo@pParams

pog <- pompExample(gompertz,envir=NULL)[[1]]

# test panelPomp class (validity function)

# test panelPomp function
c(TESTS_PASS,
  try(panelPomp(),silent=TRUE)[1]==sQuotes(
    "Error : in 'panelPomp': 'object' is a required argument.\n") -> PASSES#?
) -> TESTS_PASS
PASSES#?
c(TESTS_PASS,
  try(panelPomp(list(a=1)),silent=TRUE)[1]==sQuotes(
    "Error : in 'panelPomp': 'object' must be a either a 'panelPomp' ",
    "object or a list of 'pomp' objects.\n") -> PASSES#?
) -> TESTS_PASS
PASSES#?
c(TESTS_PASS,
  try(panelPomp(pos,sh=2*pPs$sh,sp=2*pPs$sp,params=pPs),silent=TRUE)[1]==
    sQuotes("Error : in 'panelPomp': do not specify all of 'params', 'shared'",
            " and 'specific'.\n") -> PASSES#?
) -> TESTS_PASS
PASSES#?
## object = pompList
c(TESTS_PASS,identical(
  panelPomp(pos)@pParams,
  list(shared=numeric(),specific=sapply(pos,coef))) -> PASSES) -> TESTS_PASS
PASSES#?
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
## object = pomp
c(TESTS_PASS,
  try(panelPomp(ppo,sh="sigmaX",params=pPs),silent=TRUE)[1]==
    sQuotes("Error : in 'panelPomp': if 'shared' is a character vector (or ",
            "NULL), unit specific parameters are taken from 'object'.\n"
    ) -> PASSES) -> TESTS_PASS
PASSES#?
c(TESTS_PASS,identical(
  panelPomp(ppo,sh="sigmaX")@pParams,
  list(shared=pPs$sh["sigmaX"],specific=rbind(sigmaY=pPs$sh["sigmaY"],pPs$sp))
  ) -> PASSES) -> TESTS_PASS
PASSES#?
c(TESTS_PASS,identical(
  panelPomp(ppo,sh=NULL)@pParams,
  list(shared=numeric(),
       specific=rbind(sigmaX=pPs$sh["sigmaX"],sigmaY=pPs$sh["sigmaY"],pPs$sp))
) -> PASSES) -> TESTS_PASS
PASSES#?

stopifnot(all(TESTS_PASS))
all(TESTS_PASS)#?
