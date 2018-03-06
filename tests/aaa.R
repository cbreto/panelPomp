## basic usage:
library(panelPomp,quietly=TRUE)
TESTS_PASS <- NULL 
## alternatively: assign(eval(formals(test))$all,NULL) (after defining test)
## define test function, ...
test <- function(expr1,expr2,all="TESTS_PASS",env=parent.frame(),...) 
  panelPomp:::test(expr1,expr2,all=all,env=env,...)
## ..., perform tests, and ...
test(NULL,NULL)
## check whether all tests passed
all(get(eval(formals(test))$all))
if (!all(get(eval(formals(test))$all))) stop("Not all tests passed!")

## complete tests for 'test':
## test if results are added to 'all'
test(NULL,NULL)
## if only one argument, try(,silent=T)[1] 
## (not necessarily logical; hence, all is not changed!)
test(length(get(eval(formals(test))$all))==2)
test(length(get(eval(formals(test))$all)),2L)
## test order of expr1 and expr2
test(3L,length(get(eval(formals(test))$all)))
## test stop for wrong parameters
test(wQuotes("Error in try(expr1, silent = TRUE) : object 'invalid_expr' not ",
             "found\n"),test(invalid_expr))
test(wQuotes("Error : in ''test'': missing vector to accumulate logical test ",
             "results.\n"),
     panelPomp:::test(NULL,expr2=NULL,all="wrong_all",env=parent.frame()))
test("Error in exists(all, envir = env) : invalid 'envir' argument\n",
     panelPomp:::test(NULL,NULL,all=eval(formals(test))$all,env="no_env"))
## test identical for range of objects
test(NA,NA)
test(1,1)
test(1L,1L)
test("a","a")
test(matrix(1,nrow=2),matrix(1,nrow=2))
test(list(a="a",b="b"),list(a="a",b="b"))
test(c(TRUE,TRUE),c(TRUE,TRUE))
## tests for .onAttach
test(tail(strsplit(options("pomp.examples")$pomp.examples[2],"/")[[1]],2),
     c("panelPomp","examples"))
## tests for .onDetach
detach("package:panelPomp",unload=TRUE)
is.na(options("pomp.examples")$pomp.examples[2])

## final check: do all tests pass?
all(get(eval(formals(test))$all))
if (!all(get(eval(formals(test))$all))) stop("Not all tests passed!")