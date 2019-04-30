if (file.exists("_options.R")) source("_options.R")
library(panelPomp,quietly=TRUE)

TESTS_PASS <- NULL 
test <- function(expr1,expr2,all="TESTS_PASS",env=parent.frame(),...) 
  panelPomp:::test(expr1,expr2,all=all,env=env,...)

## no warnings
test(wQuotes("<object of class ''panelPomp''>"),
     capture.output(panelPompExample(prw))[1])

##objExample <- function (example, pckg) 
## example
ep <- wQuotes("Error : in ''panelPompExample'': ")
## if (example=="") 
test(paste0("panelPomp examples in ",getOption("panelPomp.examples"),":"),
     capture.output(panelPompExample())[1])
## if no file
test(wQuotes(ep,"cannot find file ''non_example.R''\n"),
     panelPompExample(non_example))
## if multiple files
options(panelPomp.examples=c(getOption("panelPomp.examples"),
                             getOption("panelPomp.examples")))
warn <- options(warn=2)
test(grepl(wQuotes("Error : (converted from warning) in ''panelPompExample'': ",
                   "multiple instances of file ''prw.R'' were found; "),
           test(names(panelPompExample(prw))),fixed=TRUE),TRUE)
options(warn); rm(warn)

## check whether all tests passed
all(get(eval(formals(test))$all))
if (!all(get(eval(formals(test))$all))) stop("Not all tests passed!")

