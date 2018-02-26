library(panelPomp)

TESTS_PASS <- NULL

ppo <- pompExample(prw,envir=NULL)[[1]]
pos <- as(ppo,"list")
po <- pos[[1]]

TESTS_PASS <- c(
  TESTS_PASS,# for missing object
  try(panelPomp(list(a=1)),silent=TRUE)[1]==sQuotes(
        "Error : in 'panelPomp': The 'unit.objects' slot must be a list of ",
        "'pomp' objects.\n") -> PASSES#?
)
PASSES#?

stopifnot(all(TESTS_PASS))
all(TESTS_PASS)#?
