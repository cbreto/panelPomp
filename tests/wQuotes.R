library(panelPomp,quietly=TRUE)

TESTS_PASS <- NULL
test <- function(expr1,expr2,all="TESTS_PASS",env=parent.frame(),...) 
  panelPomp:::test(expr1,expr2,all=all,env=env,...)

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
## test passing sQuotes as first argument to stop
test(as.character(
  attr(try(stop(wQuotes("in ''fn'': ''object'' is a required argument")),
           silent=TRUE),"condition")),
  paste0(
    "Error in doTryCatch(return(expr), name, parentenv, handler): in ",
    sQuote("fn"),": ",sQuote("object")," is a required argument\n"))

## check whether all tests passed
all(get(eval(formals(test))$all))
if (!all(get(eval(formals(test))$all))) stop("Not all tests passed!")
