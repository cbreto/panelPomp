library(panelPomp,quietly=TRUE)

TESTS_PASS <- NULL
test <- function(expr1,expr2,all="TESTS_PASS",env=parent.frame(),...) 
  panelPomp:::test(expr1,expr2,all=all,env=env,...)

g <- pompExample(gompertz,envir=NULL)[[1]]
pg <- panelPompExample(pangomp)
pg <- panelPomp(pg[1:3])
pgl <- as(pg,"list")
coef(g)
coef(pg)
coef(panelPomp(pg,shared=NULL))
coef(panelPomp(pg,specific=names(coef(g))))
coef(panelPomp(pg,shared=c(r=0.1,sigma=0.1,K=1)))
coef(panelPomp(pg,specific=c("r","K","tau","X.0")))
try(panelPomp(pg,specific=c("tau","X.0"),params=c(r=3,K=1)))
stopifnot(all.equal(coef(panelPomp(pg,params=coef(g))),coef(g)))
try(panelPomp(pg,params=list(bob=3,nancy="A")))
try(panelPomp(pg,shared=c("r","K")))
try(panelPomp(pg,specific=c(0.3)))
coef(panelPomp(pg,specific=c(r=0.3)))
try(panelPomp(pg,specific="h"))
try(panelPomp(pg,specific="h",shared=c(r=33)))
try(panelPomp(pg,specific=list(r=0.3,K=9)))
coef(panelPomp(pg,specific=c("tau","X.0"),shared=c(r=3,K=1)))
try(panelPomp(setNames(pgl,c("a","b",""))))
coef(pgl[[2]]) <- c(h=3)
try(po <- panelPomp(pgl))
try(panelPomp(pgl[[1]]))

## check whether all tests passed
all(get(eval(formals(test))$all))
if (!all(get(eval(formals(test))$all))) stop("Not all tests passed!")
