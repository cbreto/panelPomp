library(panelPomp)

TESTS_PASS <- NULL

TESTS_PASS <- c(
  TESTS_PASS,# for single word (with ')
  sQuotes("'Error'")==paste0(sQuote("Error")) -> PASSES#?
)
PASSES#?
TESTS_PASS <- c(
  TESTS_PASS,# for single word (no ')
  sQuotes("Error")=="Error" -> PASSES#?
)
PASSES#?
TESTS_PASS <- c(
  TESTS_PASS,# for multiple words (starting with ')
  sQuotes("'Error' : in")==paste0(sQuote("Error")," : in") -> PASSES#?
)
PASSES#?
TESTS_PASS <- c(
  TESTS_PASS,# for multiple words (starting without ')
  sQuotes("Error : in 'fn'")==paste0("Error : in ",sQuote("fn")) -> PASSES#?
)
PASSES#?
TESTS_PASS <- c(
  TESTS_PASS,# for multiple ' (starting with ')
  sQuotes("'Error' : in 'fn': 'object' is a required argument")==paste0(
    sQuote("Error")," : in ",sQuote("fn"),": ",sQuote("object"),
    " is a required argument") -> PASSES#?
)
PASSES#?
TESTS_PASS <- c(
  TESTS_PASS,# for multiple ' (starting without ')
  sQuotes("Error : in 'fn': 'object' is a required argument"
  )==paste0("Error : in ",sQuote("fn"),": ",sQuote("object"),
            " is a required argument") -> PASSES#?
)
PASSES#?
TESTS_PASS <- c(
  TESTS_PASS,# for multiple arguments
  sQuotes("in 'fn'",": 'object' is"," a required argument"," Error : in",
          " 'fn': 'object' is a required argument")==
    paste0("in ",sQuote("fn"),": ",sQuote("object")," is a required argument",
           " Error : in ",sQuote("fn"),": ",sQuote("object"),
           " is a required argument") -> PASSES#?
)
PASSES#?
TESTS_PASS <- c(
  TESTS_PASS,# for use with stop()
  as.character(
    attr(try(stop(sQuotes("in 'fn': 'object' is a required argument")),
             silent=TRUE),"condition"))==
    paste0(
      "Error in doTryCatch(return(expr), name, parentenv, handler): in ",
      sQuote("fn"),": ",sQuote("object")," is a required argument\n") -> PASSES#?
)
PASSES#?

stopifnot(all(TESTS_PASS))
all(TESTS_PASS)#?
