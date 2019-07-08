
library(panelPomp,quietly=TRUE)

TESTS_PASS <- NULL
test <- function(expr1,expr2,all="TESTS_PASS",env=parent.frame(),...)
  panelPomp:::test(expr1,expr2,all=all,env=env,...)

m1 <- mcap( lp=-(-5:5)^2,parameter=-5:5,Ngrid=120)
test(round(m1$mle,1),0)

