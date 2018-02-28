library(panelPomp)

TESTS_PASS <- NULL

ppo <- pompExample(prw,envir=NULL)[[1]]
pP2 <- list(shared=c(sigmaX=1,sigmaY=2),
            specific=matrix(c(0,0.1),nr=1,
                            dimnames=list(param="X.0",unit=c("rw1","rw2"))))
ppo <- new("panelPomp",unit.objects=unitobjects(ppo),pParams=pP2)

# coef,panelPomp-method
c(TESTS_PASS,identical(coef(ppo),setNames(c(1,2,0,0.1),c("sigmaX","sigmaY",
  sprintf("X.0[rw1]"),sprintf("X.0[rw2]")))) -> PASSES) -> TESTS_PASS
PASSES#?
# coef<-,panelPomp-method
# length,panelPomp-method
c(TESTS_PASS,(length(ppo)==2) -> PASSES) -> TESTS_PASS
PASSES#?
# names,panelPomp-method
c(TESTS_PASS,identical(names(ppo),c("rw1","rw2")) -> PASSES) -> TESTS_PASS
PASSES#?
# pparams,panelPomp-method
c(TESTS_PASS,identical(pparams(ppo),pP2) -> PASSES) -> TESTS_PASS
PASSES#?
# unitobjects,panelPomp-method
c(TESTS_PASS,identical(unitobjects(ppo),ppo@unit.objects) -> PASSES
) -> TESTS_PASS
PASSES#?
c(TESTS_PASS,identical(unitobjects(ppo,unit="rw1"),ppo@unit.objects[["rw1"]]
) -> PASSES) -> TESTS_PASS
PASSES#?
# window,panelPomp-method
c(TESTS_PASS,(length(window(ppo,U=1))==1) -> PASSES) -> TESTS_PASS
PASSES#?
c(TESTS_PASS,identical(
  coef(window(ppo,U=1)),
  setNames(c(1,2,0),c("sigmaX","sigmaY",sprintf("X.0[rw1]")))) -> PASSES
) -> TESTS_PASS
PASSES#?
c(TESTS_PASS,identical(
  lapply(as(window(ppo,start=2),"list"),time),list(rw1=c(2,3,4),rw2=c(2,3,4))
  ) -> PASSES) -> TESTS_PASS
PASSES#?
c(TESTS_PASS,identical(
  lapply(as(window(ppo,end=2),"list"),time),list(rw1=c(1,2),rw2=c(1,2))
) -> PASSES) -> TESTS_PASS
PASSES#?
c(TESTS_PASS,(length(window(ppo,U=1,start=1,end=2))==1) -> PASSES
) -> TESTS_PASS
PASSES#?
c(TESTS_PASS,identical(
  lapply(as(window(ppo,U=1,start=1,end=2),"list"),time),list(rw1=c(1,2))
) -> PASSES) -> TESTS_PASS
PASSES#?

stopifnot(all(TESTS_PASS))
all(TESTS_PASS)#?
