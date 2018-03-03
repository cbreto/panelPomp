library(panelPomp,quietly=TRUE)

TESTS_PASS <- NULL
test <- function(expr1,expr2,all="TESTS_PASS",env=parent.frame(),...) 
  panelPomp:::test(expr1,expr2,all=all,env=env,...)

ppo <- pompExample(prw,envir=NULL)[[1]]
pP2 <- list(shared=c(sigmaX=1,sigmaY=2),
            specific=matrix(c(0,0.1),nr=1,
                            dimnames=list(param="X.0",unit=c("rw1","rw2"))))
ppo <- panelPomp(unitobjects(ppo),params=pP2)

## test coef,panelPomp-method
test(coef(ppo),setNames(c(1,2,0,0.1),c("sigmaX","sigmaY",
                                       sprintf("X.0[rw1]"),sprintf("X.0[rw2]"))))
## coef<-,panelPomp-method
test(coef(ppo),{coef(ppo) <- coef(ppo);coef(ppo)})
wQuotes("Error : in ''coef<-'': part of ''value'' is not part of ",
               "''coef(object)''.\n") -> err
test(coef(ppo) <- c(ppo@pParams$shared,xsh=5),err)
test(coef(ppo) <- c(coef(ppo),xsh=5),err)
test({coef(ppo) <- setNames(
  c(coef(ppo),5,6),c(names(coef(ppo)),
                     sprintf("xsp[rw1]"),sprintf("xsp[rw2]")))},err)
test({coef(ppo) <- setNames(
  c(coef(ppo),5,6,7),c(names(coef(ppo)),
                       sprintf("xsp[rw1]"),sprintf("xsp[rw2]"),"xsh"))},err)
test({coef(ppo) <- setNames(
  c(coef(ppo)[-c(1:2)],5,6),c(names(coef(ppo)[-c(1:2)]),
                              sprintf("xsp[rw1]"),sprintf("xsp[rw2]")))},err)
wQuotes("Error : in ''coef<-'': part of ''coef(object)'' is not specified ",
               "in ''value''.\n") -> err
test(coef(ppo) <- coef(ppo)[-c(1:2)],err)
test(coef(ppo) <- ppo@pParams$shared,err)
#err <- wQuotes(
#  "Error : in 'coef<-': invalid class ",dQuote("panelPomp")," object: All ",
#  "parameters in the pomp objects of 'unit.objects' slot must be in 'pParams'",
#  "and viceversa (validity check)\n")
## test length,panelPomp-method
test(length(ppo),2L)
## test names,panelPomp-method
test(names(ppo),c("rw1","rw2"))
## test pparams,panelPomp-method
test(pparams(ppo),ppo@pParams)
## test pParams function
## all sh
test(pParams(coef(ppo)[grep("^.+\\[.+?\\]$",names(coef(ppo)),perl=TRUE,
                            value=TRUE,invert=TRUE)]),
     list(shared=ppo@pParams$shared,specific=array(numeric(0),dim=c(0,0))))
## all sp
test(list(shared=numeric(0),specific=ppo@pParams$specific),
     pParams(coef(ppo)[grep("^.+\\[.+?\\]$",names(coef(ppo)),perl=TRUE,
                            value=TRUE)]))
## both sh & sp
test(pParams(coef(ppo)),ppo@pParams)
## test unitobjects,panelPomp-method
test(unitobjects(ppo),ppo@unit.objects)
test(unitobjects(ppo,unit="rw1"),ppo@unit.objects[["rw1"]])
## test window,panelPomp-method
test(length(window(ppo,U=1))==1L)
test(setNames(c(1,2,0),c("sigmaX","sigmaY",sprintf("X.0[rw1]"))),
     coef(window(ppo,U=1)))
test(lapply(as(window(ppo,start=2),"list"),time),list(rw1=c(2,3,4),rw2=c(2,3,4)))
test(lapply(as(window(ppo,end=2),"list"),time),list(rw1=c(1,2),rw2=c(1,2)))
test(length(window(ppo,U=1,start=1,end=2)),1L) 
test(lapply(as(window(ppo,U=1,start=1,end=2),"list"),time),list(rw1=c(1,2)))

## check whether all tests passed
all(get(eval(formals(test))$all))
if (!all(get(eval(formals(test))$all))) stop("Not all tests passed!")
