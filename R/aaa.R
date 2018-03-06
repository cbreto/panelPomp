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

test <- function (expr1, expr2, all, env, verbose = TRUE) {
  # expr1: expression to be try(,sil=T)[1]d
  # expr2: optional; compare to 'expr1' (via identical) and c() logical result
  #     to object specified via arguments 'all' and 'env' (see below)
  # all: name of the vector to accumulate logical test results
  # env: name of the environment where 'all' should be modified
  # verbose: optional; should the result be returned?
  ep <- paste0("in ",sQuote("test"),": ")
  if (!exists(all,envir=env))
    stop(paste0(ep,"missing vector to accumulate logical ",
                "test results."),call.=FALSE)
  tryexpr1 <- try(expr1,silent=TRUE)
  if (is(tryexpr1,"try-error")) tryexpr1 <- tryexpr1[1]
  PASSES <- tryexpr1
  if (!missing(expr2)) {
    tryexpr2 <- try(expr2,silent=TRUE)
    if (is(tryexpr2,"try-error")) tryexpr2 <- tryexpr2[1]
    PASSES <- identical(tryexpr1,tryexpr2)
    assign(all,value=c(get(all,envir=env),PASSES),envir=env)
  }
  if (verbose) PASSES
}
