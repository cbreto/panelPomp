library(panelPomp)

TESTS_PASS <- NULL
test <- function(...,all="TESTS_PASS",env=parent.frame()) 
  panelPomp:::test(...,all=all,env=env)

ppo <- pompExample(prw,envir=NULL)[[1]]
pP2 <- list(shared=c(sigmaX=1,sigmaY=2),
            specific=matrix(c(0,0.1),nr=1,
                            dimnames=list(param="X.0",unit=c("rw1","rw2"))))
ppo <- new("panelPomp",unit.objects=unitobjects(ppo),pParams=pP2)

## test coef,panelPomp-method
test(identical(
  coef(ppo),setNames(c(1,2,0,0.1),c("sigmaX","sigmaY",
                                    sprintf("X.0[rw1]"),sprintf("X.0[rw2]")))))
#
## coef<-,panelPomp-method
# tests for 'coef<-' with right and wrong parameter configurations
#pParams(ppo@pParams$shared)# all sh ()
#coef(ppo) # both
#setNames(c(coef(ppo),5),c(names(coef(ppo)),"xsh"))
#setNames(c(coef(ppo),5,6,7),c(names(coef(ppo)),sprintf("xsp[rw1]"),sprintf("xsp[rw2]"),"xsh"))
#coef(ppo)[-c(1:2)] # all sp
#setNames(c(coef(ppo)[-c(1:2)],5,6),c(names(coef(ppo)[-c(1:2)]),sprintf("xsp[rw1]"),sprintf("xsp[rw2]")))
test(try(coef(ppo) <- c(ppo@pParams$shared,xsh=5),silent=TRUE)[1]==sQuotes(
  "Error : in 'coef<-': part of 'value' is not part of 'coef(object)'.\n"))
test(try(coef(ppo) <- setNames(
  c(coef(ppo),5,6),c(names(coef(ppo)),
                     sprintf("xsp[rw1]"),sprintf("xsp[rw2]"))),
  silent=TRUE)[1]==sQuotes(
    "Error : in 'coef<-': part of 'value' is not part of 'coef(object)'.\n"))
## test length,panelPomp-method
test(length(ppo)==2)
## test names,panelPomp-method
test(identical(names(ppo),c("rw1","rw2")))
## test pparams,panelPomp-method
test(identical(pparams(ppo),pP2))
test(identical(
  pParams(ppo@pParams$shared), ## all sh
  list(shared=ppo@pParams$shared,specific=array(numeric(0),dim=c(0,0)))))
test(identical(
  pParams(ppo@pParams$shared), ## all sh
  list(shared=ppo@pParams$shared,specific=array(numeric(0),dim=c(0,0)))))
test(identical(
  pParams(coef(ppo)[-c(1:2)]), ## all sp
  list(shared=numeric(0),specific=ppo@pParams$specific)))
test(identical(pParams(coef(ppo)),ppo@pParams)) ## both sh & sp
## test unitobjects,panelPomp-method
test(identical(unitobjects(ppo),ppo@unit.objects))
test(identical(unitobjects(ppo,unit="rw1"),ppo@unit.objects[["rw1"]]))
## test window,panelPomp-method
test(length(window(ppo,U=1))==1)
test(identical(
  coef(window(ppo,U=1)),
  setNames(c(1,2,0),c("sigmaX","sigmaY",sprintf("X.0[rw1]")))))
test(identical(
  lapply(as(window(ppo,start=2),"list"),time),list(rw1=c(2,3,4),rw2=c(2,3,4))))
test(identical(
  lapply(as(window(ppo,end=2),"list"),time),list(rw1=c(1,2),rw2=c(1,2)))) 
test(length(window(ppo,U=1,start=1,end=2))==1) 
test(identical(
  lapply(as(window(ppo,U=1,start=1,end=2),"list"),time),list(rw1=c(1,2))))

## check whether all tests passed
all(get(eval(formals(test))$all))
if (!all(get(eval(formals(test))$all))) stop("Not all tests passed!")
