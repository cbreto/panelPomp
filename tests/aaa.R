## test aaa.R

if (file.exists("_options.R")) source("_options.R")
library(panelPomp,quietly=TRUE)
TESTS_PASS <- NULL 
## alternatively: assign(eval(formals(test))$all,NULL) (after defining test)
## does panelPomp:::test (used to test all other codes) work?
test <- function(expr1,expr2,all="TESTS_PASS",env=parent.frame(),...) 
  panelPomp:::test(expr1,expr2,all=all,env=env,...)

test(NULL,NULL)
## ... test unevaluated multiple-line expression, ...
test(
  quote(
    {a_multi_line_expression <- NA
    "where_objects_are_defined" -> is_not_evald
    NULL}),NULL)
test(exists("is_not_evald"),FALSE)
## if only one argument ...
test(length(get(eval(formals(test))$all))==3)
## ... the result isn't necessarily logical: hence, 'all' is not changed!
test(length(get(eval(formals(test))$all)),3L)
## test order of expr1 and expr2
test(4L,length(get(eval(formals(test))$all)))
## test stop for wrong parameters
test(wQuotes("Error in eval(expr1) : object 'invalid_expr' not found\n"),
     test(invalid_expr))
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
test(c(TRUE,FALSE),c(TRUE,FALSE))
## consider further testing panelPomp:::test ...
## capturing warnings, ...
## warn <- options(warn=2) # to convert warnings to errors
## test()
## options(warn)
## partially matching error messages, ...
## test(grepl(wQuotes("Error "),test(),fixed=TRUE),TRUE)

## if all tests for panelPomp:::test passed ...
all(get(eval(formals(test))$all))
if (!all(get(eval(formals(test))$all))) stop("Not all tests passed!")
## ... continue testing the rest of the code


## runif.EstimationScale
test(class(panelPomp:::runif.EstimationScale(centers=c(th=1),widths=2))[1],
     "numeric")

## wQuotes
## check for ' in different positions in the character
test(wQuotes("''Error''")==paste0(sQuote("Error")))
test(wQuotes("Error")=="Error")
test(wQuotes("''Error'' : in")==paste0(sQuote("Error")," : in"))
test(wQuotes("Error : in ''fn''")==paste0("Error : in ",sQuote("fn")))
test(wQuotes("''Error'' : in ''fn'': ''object'' is a required argument"),
     paste0(sQuote("Error")," : in ",sQuote("fn"),": ",sQuote("object"),
            " is a required argument"))
test(wQuotes("Error : in ''fn'': ''object'' is a required argument"),
     paste0("Error : in ",sQuote("fn"),": ",sQuote("object"),
            " is a required argument"))
test(wQuotes("in ''fn''",": ''object'' is"," a required argument"," Error : in",
             " ''fn'': ''object'' is a required argument"),
     paste0("in ",sQuote("fn"),": ",sQuote("object")," is a required argument",
            " Error : in ",sQuote("fn"),": ",sQuote("object"),
            " is a required argument"))
## test passing wQuotes as first argument to stop
test(as.character(
  attr(try(stop(wQuotes("in ''fn'': ''object'' is a required argument")),
           silent=TRUE),"condition")),
  paste0(
    "Error in doTryCatch(return(expr), name, parentenv, handler): in ",
    sQuote("fn"),": ",sQuote("object")," is a required argument\n"))

## test quoting variables 
test(wQuotes("''",TESTS_PASS[1],"''")==sQuote("TRUE"))

## final check: do all tests pass?
all(get(eval(formals(test))$all))
if (!all(get(eval(formals(test))$all))) stop("Not all tests passed!")
