## basic usage:
library(panelPomp,quietly=TRUE)
TESTS_PASS <- NULL 
## alternatively: assign(eval(formals(test))$all,NULL) (after defining test)
## define test function, ...
test <- function(expr,all="TESTS_PASS",env=parent.frame(),...) 
  panelPomp:::test(expr,all=all,env=env,...)
## ..., perform tests, and ...
test(identical(NULL,NULL))
## check whether all tests passed
all(get(eval(formals(test))$all))
if (!all(get(eval(formals(test))$all))) stop("Not all tests passed!")



## refresh <- expression({
##   rm(list=ls()[!(ls() %in% c('test','TESTS_PASS','refresh'))])
##   refreshed <- NULL
## })
## eval(refresh)



## complete tests for 'test':
## test if results are added to 'all'
test(identical(NULL,NULL))
test(length(get(eval(formals(test))$all))==2)
## test stop for wrong parameters
test(identical(
  try(test(invalid_expr),silent=TRUE)[1],paste0(sQuotes(
    "Error : in 'test': object")," 'invalid_expr' not found\n")))
test(identical(
  try(test(NULL),silent=TRUE)[1],sQuotes(
    "Error : in 'test': 'expr' does not evaluate to an object of class ",
    "'logical'.\n")))
test_test <- function(expr,all="wrong_all",env=parent.frame(),...) 
  panelPomp:::test(expr,all=all,env=env,...)
test(identical(
  try(test_test(identical(NULL,NULL)),silent=TRUE)[1],sQuotes(
    "Error : in 'test': missing vector to accumulate logical test results.\n")))
test_test <- function(expr,all=eval(formals(test))$all,env="wrong_env",...) 
  panelPomp:::test(expr,all=all,env=env,...)
test(identical(
  try(test_test(identical(NULL,NULL)),silent=TRUE)[1],
    "Error in exists(all, envir = env) : invalid 'envir' argument\n"))
rm(test_test)

## final check: do all tests pass?
all(get(eval(formals(test))$all))
if (!all(get(eval(formals(test))$all))) stop("Not all tests passed!")
