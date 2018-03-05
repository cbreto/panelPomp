library(panelPomp,quietly=TRUE)

TESTS_PASS <- NULL 
test <- function(expr1,expr2,all="TESTS_PASS",env=parent.frame(),...) 
  panelPomp:::test(expr1,expr2,all=all,env=env,...)

## write rows in r= & columns in c=
m1x1 <- matrix(
  paste0(rep(paste0(1:c(r=1),"_"),times=c(c=1)),rep(seq(1,c(c=1)),each=c(r=1))),
  nrow=c(r=1),dimnames=list(paste0("r",1:c(r=1)),c(paste0("c",1:c(c=1)))))
m1x2 <- matrix( ## named rownames and colnames!
  paste0(rep(paste0(1:c(r=1),"_"),times=c(c=2)),rep(seq(1,c(c=2)),each=c(r=1))),
  nrow=c(r=1),dimnames=list(rnm=paste0("r",1:c(r=1)),cnm=c(paste0("c",1:c(c=2)))))
m2x5 <- matrix(
  paste0(rep(paste0(1:c(r=2),"_"),times=c(c=5)),rep(seq(1,c(c=5)),each=c(r=2))),
  nrow=c(r=2),dimnames=list(paste0("r",1:c(r=2)),c(paste0("c",1:c(c=5)))))

# tests for get_col
############################################[rows,col]###############[rows]
test(get_col(m1x1,rows=1,col=1),
     setNames(m1x1[rows=1,col=1],nm=rownames(m1x1)[rows=1]))
test(get_col(m1x2,rows=1,col=1),
     setNames(m1x2[rows=1,col=1],nm=rownames(m1x2)[rows=1]))
test(get_col(m1x2,rows=1,col=1),
     setNames(m1x2[rows=1,col=1],nm=rownames(m1x2)[rows=1]))
test(get_col(m2x5,rows=1,col=1),
     setNames(m2x5[rows=1,col=1],nm=rownames(m2x5)[rows=1]))
test(get_col(m2x5,,col=1),setNames(m2x5[,col=1],nm=rownames(m2x5)[]))
# tests for get_row
############################################[rows,col]###############[rows]
test(get_row(m1x1,cols=1,row=1),
     setNames(m1x1[row=1,cols=1],nm=colnames(m1x1)[cols=1]))
test(get_row(m2x5,cols=1,row=1),
     setNames(m2x5[row=1,cols=1],nm=colnames(m2x5)[cols=1]))
test(get_row(m2x5,,row=1),setNames(m2x5[row=1,],nm=colnames(m2x5)[]))

## check whether all tests passed
all(get(eval(formals(test))$all))
if (!all(get(eval(formals(test))$all))) stop("Not all tests passed!")
