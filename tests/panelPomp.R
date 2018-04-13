library(panelPomp,quietly=TRUE)

TESTS_PASS <- NULL
test <- function(expr1,expr2,all="TESTS_PASS",env=parent.frame(),...) 
  panelPomp:::test(expr1,expr2,all=all,env=env,...)

g <- pompExample(gompertz,envir=NULL)[[1]]
pg <- panelPompExample(pangomp)
pg <- panelPomp(window(pg,U=3))
coef(g)
coef(pg)
coef(panelPomp(pg,shared=NULL))
coef(panelPomp(pg,specific=names(coef(g))))
coef(panelPomp(pg,shared=c(r=0.1,sigma=0.1,K=1)))
coef(panelPomp(pg,specific=c("r","K","tau","X.0")))
test(coef(panelPomp(pg,params=coef(g))),coef(g))

## check whether all tests passed
all(get(eval(formals(test))$all))
if (!all(get(eval(formals(test))$all))) stop("Not all tests passed!")
