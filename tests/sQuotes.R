library(panelPomp)

TESTS_PASS <- NULL

TESTS_PASS <- c(
  TESTS_PASS,# for single word (with ')
  sQuotes("'Error'")=="‘Error’" -> PASSES#?
)
PASSES#?
TESTS_PASS <- c(
  TESTS_PASS,# for single word (no ')
  sQuotes("Error")=="Error" -> PASSES#?
)
PASSES#?
TESTS_PASS <- c(
  TESTS_PASS,# for multiple words (starting with ')
  sQuotes("'Error' : in")=="‘Error’ : in" -> PASSES#?
)
PASSES#?
TESTS_PASS <- c(
  TESTS_PASS,# for multiple words (starting without ')
  sQuotes("Error : in 'fn'")=="Error : in ‘fn’" -> PASSES#?
)
PASSES#?
TESTS_PASS <- c(
  TESTS_PASS,# for multiple ' (starting with ')
  sQuotes(
    "'Error' : in 'fn': 'object' is a required argument"
  )=="‘Error’ : in ‘fn’: ‘object’ is a required argument" -> PASSES#?
)
PASSES#?
TESTS_PASS <- c(
  TESTS_PASS,# for multiple ' (starting without ')
  sQuotes("Error : in 'fn': 'object' is a required argument"
  )=="Error : in ‘fn’: ‘object’ is a required argument" -> PASSES#?
)
PASSES#?
TESTS_PASS <- c(
  TESTS_PASS,# for multiple arguments
  sQuotes("in 'fn'",": 'object' is"," a required argument"," Error : in",
          " 'fn': 'object' is a required argument")==
    paste0("in ‘fn’: ‘object’ is a required argument Error : in",
           " ‘fn’: ‘object’ is a required argument") -> PASSES#?
)
PASSES#?
TESTS_PASS <- c(
  TESTS_PASS,# for use with stop()
  as.character(
    attr(try(stop(sQuotes("in 'fn': 'object' is a required argument")),
             silent=TRUE),"condition"))==
    paste0(
      "Error in doTryCatch(return(expr), name, parentenv, handler): in ‘fn’: ",
      "‘object’ is a required argument\n") -> PASSES#?
)
PASSES#?

all(TESTS_PASS)#?
