library(panelPomp,quietly=TRUE)

TESTS_PASS <- NULL 
test <- function(expr1,expr2,all="TESTS_PASS",env=parent.frame(),...) 
  panelPomp:::test(expr1,expr2,all=all,env=env,...)

## basic checks for logLik, matrix-method
repMargin <- 1
unit1.ll <- 1
unit2.ll <- 10
object <- ulls <- matrix(c(unit1.ll,unit1.ll,unit2.ll,unit2.ll),nr=2,nc=2,
                         byrow=ifelse(repMargin==1,FALSE,TRUE))
exp.ll <- unit1.ll + unit2.ll
exp.se <- 0

chck.avl <- logLik(ulls,repMargin=repMargin,first="aver",aver="logmeanexp",se=TRUE)
chck.avm <- logLik(ulls,repMargin=repMargin,first="aver",aver="mean",se=TRUE)
chck.agl <- logLik(ulls,repMargin=repMargin,first="aggr",aver="logmeanexp",se=TRUE)
chck.agm <- logLik(ulls,repMargin=repMargin,first="aggr",aver="mean",se=TRUE)

test(TRUE,all(c(
  chck.avl==c(exp.ll,exp.se),
  chck.avm==c(exp.ll,exp.se),
  chck.agl==c(exp.ll,exp.se),
  chck.agm==c(exp.ll,exp.se))))

## test first='aver' & aver='logmeanexp' for logLik, matrix-method
repMargin <- 2
unit1.ll <- 2
unit2.ll <- 10
object <- ulls <- matrix(
  c(log(0.5*exp(unit1.ll)),log(1.5*exp(unit1.ll)),unit2.ll,unit2.ll),
  nr=2,nc=2,byrow=ifelse(repMargin==1,FALSE,TRUE)
)
chck <- logLik(ulls,repMargin=repMargin,first="aver",aver="logmeanexp",se=TRUE)
test(unname(chck[1]),c(unit1.ll + unit2.ll))

## test aver='mean' for logLik, matrix-method
repMargin <- 1
unit1.ll <- 1
unit2.ll <- 10
object <- ulls <- matrix(
  c(unit1.ll+0.5,unit1.ll-0.5,unit2.ll,unit2.ll),
  nr=2,nc=2,byrow=ifelse(repMargin==1,FALSE,TRUE)
)
chck <- logLik(ulls,repMargin=repMargin,first="aggr",aver="mean",se=TRUE)
test(unname(chck[1]),c(unit1.ll + unit2.ll))

## test panel_logmeanexp
repMargin <- 1
ul1 <- 1
ul2 <- 10
uls <- matrix(c(ul1,ul1,ul2,ul2),nr=2,byrow=ifelse(repMargin==1,FALSE,TRUE))
expected_output <- setNames(c(ul1+ul2,0),nm=c("","se"))

test(panel_logmeanexp(uls,MARGIN=ifelse(repMargin==1,2,1),se=FALSE),
     unname(expected_output[1]))
test(panel_logmeanexp(uls,MARGIN=ifelse(repMargin==1,2,1),se=TRUE),
     expected_output)

## check whether all tests passed
all(get(eval(formals(test))$all))
if (!all(get(eval(formals(test))$all))) stop("Not all tests passed!")
