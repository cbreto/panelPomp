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

test <- function (..., all, env, verbose=TRUE) {
  # all: name of the vector to accumulate logical test results
  # env: name of the environment where 'all' should be modified
  if (!exists(all,envir=env))
    stop(paste0("in ",sQuote("test"),": missing vector to accumulate logical ",
                "test results."),call.=FALSE)
  PASSES <- eval(...)
  assign(all,value=c(get(all),PASSES),envir=env)
  if (verbose) PASSES
}
