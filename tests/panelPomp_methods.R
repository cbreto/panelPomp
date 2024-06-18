## test codes in R/panelPomp_methods.R
if (file.exists("_options.R")) source("_options.R")
library(panelPomp,quietly=TRUE)

TESTS_PASS <- NULL
test <- function(expr1,expr2,all="TESTS_PASS",env=parent.frame(),...)
  panelPomp:::test(expr1,expr2,all=all,env=env,...)

ppo <- panelRandomWalk(U=2,N=5)
pP2 <- list(shared=c(sigmaX=1,sigmaY=2),
            specific=matrix(c(0,0.1),nr=1,
                            dimnames=list(param="X.0",unit=c("rw1","rw2"))))
ppo <- panelPomp(unit_objects(ppo),shared=pP2$shared,specific=pP2$specific)
# other definitions from old test file
pg <- panelGompertz(U=3,N=5)
pgl <- as(pg,"list")
g <- pgl[[1]]; coef(g) <- c(pparams(pg)$sh, pparams(pg)$sp[,1])
pp <- panelPomp(list(g,g),shared=pg@shared,
                specific=pg@specific[,1:2])

## coef,panelPomp-method
test(coef(ppo),
     setNames(c(1,2,0,0.1),c("sigmaX","sigmaY","X.0[rw1]","X.0[rw2]")))

## coef<-,panelPomp-method
test(coef(ppo),{coef(ppo) <- 2*coef(ppo);coef(ppo) <- coef(ppo)/2;coef(ppo)})
test(coef(ppo),
     {coef(ppo) <- as.list(
       setNames(c(1,2,0,0.1),c("sigmaX","sigmaY","X.0[rw1]","X.0[rw2]")))
     coef(ppo)})
wQuotes("Error : in ''coef<-'': part of ''value'' is not part of ",
               "''coef(object)''.\n") -> err
test(coef(ppo) <- c(ppo@shared,xsh=5),err)
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
test(coef(ppo) <- ppo@shared,err)
## test length,panelPomp-method
test(length(ppo),2L)
## test names,panelPomp-method
test(names(ppo),c("rw1","rw2"))
## test pparams,panelPomp-method
test(pparams(ppo),list(shared=ppo@shared,specific=ppo@specific))
## test pParams function
## all sh
test(pParams(coef(ppo)[grep("^.+\\[.+?\\]$",names(coef(ppo)),perl=TRUE,
                            value=TRUE,invert=TRUE)]),
     list(shared=ppo@shared,specific=array(numeric(0),dim=c(0,0))))
## all sp
test(list(shared=numeric(0),specific=ppo@specific),
     pParams(coef(ppo)[grep("^.+\\[.+?\\]$",names(coef(ppo)),perl=TRUE,
                            value=TRUE)]))
## both sh & sp
test(pParams(coef(ppo)),list(shared=ppo@shared,specific=ppo@specific))

# Test error message if pParams used on data.frame
test(
  wQuotes("Error : in ''pParams'': ", "input must be a vector.\n"),
  pParams(data.frame('par1' = 1, 'par2' = 2))
)

## test unit_objects,panelPomp-method
test(unit_objects(ppo),ppo@unit_objects)
coef(ppo[["rw1"]])
## test print function (tested in 'print-results.Rout.save')
## test show function (tested in 'print-results.Rout.save')
## test window,panelPomp-method
test(length(ppo[1])==1L)
test(setNames(c(1,2,0),c("sigmaX","sigmaY",sprintf("X.0[rw1]"))),
     coef(ppo[1]))
test(lapply(as(window(ppo,start=2),"list"),time),list(rw1=c(2,3,4,5),rw2=c(2,3,4,5)))
test(lapply(as(window(ppo,end=2),"list"),time),list(rw1=c(1,2),rw2=c(1,2)))
test(length(window(ppo[1:2],start=1,end=2)),2L)
test(lapply(as(window(ppo[1],start=1,end=2),"list"),time),list(rw1=c(1,2)))



## as(,'list') returns list of units with parameters
test(as(pg,"list")[[1]]@data,pg@unit_objects[[1]]@data)

## as(,'pompList')
test(as(pg,'pompList')[[1]]@data,pg@unit_objects[[1]]@data)

## test as(,'data.frame')
test(dim(as(pg,"data.frame")),c(15L,4L))
test(names(as(pg,"data.frame")),c("t","Y","X","unit"))

## show
show(ppo)
show(panelPomp(unit_objects(ppo)))

## check whether all tests passed
all(get(eval(formals(test))$all))
if (!all(get(eval(formals(test))$all))) stop("Not all tests passed!")
