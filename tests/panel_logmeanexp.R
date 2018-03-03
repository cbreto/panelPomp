library(panelPomp,quietly=TRUE)

TESTS_PASS <- NULL 
test <- function(expr1,expr2,all="TESTS_PASS",env=parent.frame(),...) 
  panelPomp:::test(expr1,expr2,all=all,env=env,...)

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
