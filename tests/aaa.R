## basic usage:
library(panelPomp)
TESTS_PASS <- NULL 
## alternatively: assign(eval(formals(test))$all,NULL) (after defining test)
## define test function, ...
test <- function(...,all="TESTS_PASS",env=parent.frame()) 
  panelPomp:::test(...,all=all,env=env)
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
## test stop for wrong 'all'
test_test <- function(...,all="DOESNT_EXIST",env=parent.frame()) 
  panelPomp:::test(...,all=all,env=env)
test(identical(
  try(test_test(identical(NULL,NULL)),silent=TRUE)[1],sQuotes(
    "Error : in 'test': missing vector to accumulate logical test results.\n")))
rm(test_test)
## test if results are added to 'all'
test(length(get(eval(formals(test))$all))==2)

## final check: do all tests pass?
all(get(eval(formals(test))$all))
if (!all(get(eval(formals(test))$all))) stop("Not all tests passed!")
