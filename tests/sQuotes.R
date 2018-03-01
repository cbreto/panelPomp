library(panelPomp,quietly=TRUE)

TESTS_PASS <- NULL
test <- function(expr,all="TESTS_PASS",env=parent.frame(),...) 
  panelPomp:::test(expr,all=all,env=env,...)

## check for ' in different positions in the character
test(sQuotes("'Error'")==paste0(sQuote("Error")))
test(sQuotes("Error")=="Error")
test(sQuotes("'Error' : in")==paste0(sQuote("Error")," : in"))
test(sQuotes("Error : in 'fn'")==paste0("Error : in ",sQuote("fn")))
test(sQuotes("'Error' : in 'fn': 'object' is a required argument")==paste0(
    sQuote("Error")," : in ",sQuote("fn"),": ",sQuote("object"),
    " is a required argument"))
test(sQuotes("Error : in 'fn': 'object' is a required argument"
)==paste0("Error : in ",sQuote("fn"),": ",sQuote("object"),
          " is a required argument"))
test(sQuotes("in 'fn'",": 'object' is"," a required argument"," Error : in",
             " 'fn': 'object' is a required argument")==
    paste0("in ",sQuote("fn"),": ",sQuote("object")," is a required argument",
           " Error : in ",sQuote("fn"),": ",sQuote("object"),
           " is a required argument"))
## test passing sQuotes as first argument to stop
test(as.character(
    attr(try(stop(sQuotes("in 'fn': 'object' is a required argument")),
             silent=TRUE),"condition"))==
    paste0(
      "Error in doTryCatch(return(expr), name, parentenv, handler): in ",
      sQuote("fn"),": ",sQuote("object")," is a required argument\n"))

## check whether all tests passed
all(get(eval(formals(test))$all))
if (!all(get(eval(formals(test))$all))) stop("Not all tests passed!")
