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

test <- function (expr, all, env, verbose=TRUE, ...) {
  # expr: expression to be eval()d
  # all: name of the vector to accumulate logical test results
  # env: name of the environment where 'all' should be modified
  # ...: arguments to be passed to eval()
  ep <- paste0("in ",sQuote("test"),": ")
  if (!exists(all,envir=env))
    stop(paste0(ep,"missing vector to accumulate logical ",
                "test results."),call.=FALSE)
  PASSES <- tryCatch(eval(expr,...),
                     error=function (e) {stop(ep,conditionMessage(e),
                                              call.=FALSE)})
  if (!is.logical(PASSES))
    stop(paste0("in ",sQuote("test"),": ",sQuote("expr")," does not evaluate ",
                "to an object of class ",sQuote("logical"),"."),call.=FALSE)
  assign(all,value=c(get(all),PASSES),envir=env)
  if (verbose) PASSES
}
