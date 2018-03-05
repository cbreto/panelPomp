#' @include package.R
NULL

.onAttach <- function (...) {
  exampleDir <- getOption("pomp.examples")
  newDir <- system.file("examples",package="panelPomp")
  options(pomp.examples=c(exampleDir,newDir,recursive=TRUE))
}

.onDetach <- function (...) {
  exampleDir <- getOption("pomp.examples")
  newDir <- system.file("examples",package="panelPomp")
  exampleDir <- exampleDir[exampleDir!=newDir]
  options(pomp.examples=exampleDir)
}

test <- function (expr1, expr2, all, env, verbose = TRUE, ...) {
  # expr1: expression to be eval()d (if missing expr2)
  # expr2: optional; compare to expr1 (via identical(try(expr1),try(expr2)))
  # all: name of the vector to accumulate logical test results
  # env: name of the environment where 'all' should be modified
  # verbose: optional; should the test output be returned?
  # ...: arguments to be passed to eval()
  ep <- paste0("in ",sQuote("test"),": ")
  if (!exists(all,envir=env))
    stop(paste0(ep,"missing vector to accumulate logical ",
                "test results."),call.=FALSE)
  if (missing(expr2)) {
    expr <- expression(expr1)
  } else {
    tryexpr1 <- try(expr1,silent=TRUE)
    if (is(tryexpr1,"try-error")) tryexpr1 <- tryexpr1[1]
    tryexpr2 <- try(expr2,silent=TRUE)
    if (is(tryexpr2,"try-error")) tryexpr2 <- tryexpr2[1]
    expr <- expression(identical(tryexpr1,tryexpr2))
    }
  PASSES <- tryCatch(eval(expr,...),
                     error=function (e) {stop(ep,conditionMessage(e),
                                              call.=FALSE)})
  if (!is.logical(PASSES))
    stop(paste0("in ",sQuote("test"),": non-logical test result!"),call.=FALSE)
  assign(all,value=c(get(all,envir=env),PASSES),envir=env)
  if (verbose) PASSES
}
